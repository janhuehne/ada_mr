with Ada.Text_IO;


with Utility;
use Utility;

--with Server;
with Logger;

--with Worker;
--with Xml_Queue;
with Xml_Helper;
with GNAT.Sockets;

with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with GNAT.MD5;

package body Master is
  
  task body Master_Task is
    Master_Server_Task : Server.Server.Server_Task;
    Me                 : Master_Task_Access;
    Observer_Task      : Observer.Observer_Task;
  begin
    loop
      select
        accept Start(M : Master_Task_Access) do
          Me := M;
        end Start;
        
        Split_Raw_Data;
        
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("-> Importing jobs ...");
        
        loop
          declare
          begin
            Jobs.Add(Get_Next_Raw_Job);
          exception
            when CONSTRAINT_ERROR => exit;
          end;
        end loop;
        
        Ada.Text_IO.Put_Line("   .. Done! " & Jobs.Count'Img & " jobs imported");
        
        Master_Server_Task.Start(
          Master_Helper.Server_Bind_Ip, 
          Master_Helper.Server_Bind_Port
        );
        
        Observer_Task.Start(Me);
      or
        accept Stop;
        Ada.Text_IO.Put_Line(" -> Please wait, while closing the client connections.");
        Master_Helper.Aborted.Set_Exit;
        Master_Server_Task.Stop;
        Observer_Task.Stop;
        exit;
      end select;
    end loop;
  end Master_Task;
  
  
  
  ----------------------------------------------------
  -- GENERIC OBSERVER TASK                           -
  ----------------------------------------------------
  function Exit_Observer return Boolean is
  begin
    if Master_Helper.Aborted.Get_Exit = true OR Master_Helper.Aborted.Get_Abort = true then
      return true;
    end if;
      
    return false;
  end Exit_Observer;
  
  
  function Observe(To_Controll : Master_Task_Access) return Boolean is
    use GNAT.Sockets;
  begin
    if Jobs.Count_By_State(Master_Helper.Done) = Jobs.Count then
    
      Ada.Text_IO.Put_Line("[MASTER OBSERVER] All jobs done!");
      
      declare
        Response : String := Utility.Send(
          Master_Helper.Reducer_Ip,
          Master_Helper.Reducer_Port,
          Xml_Helper.Xml_Command(Xml_Helper.Master, "finalize")
        );
      begin
        Ada.Text_IO.Put_Line(Response);
      end;
    
      return true;
    
    end if;
      
    return false;
  end Observe;



  function "="(Left, Right : Job_Entry_Record_Access) return Boolean is
  begin
    return true;
  end "=";
  
  
  
  protected body Jobs is
  
    procedure Add(Job : My_Job) is
      Job_Entry : Job_Entry_Record_Access := new Job_Entry_Record;
    begin
      Job_Entry.Job := Job;
      Job_Entry.State := Master_Helper.Pending;
        
      Jobs.Append(Job_Entry);
      Ada.Text_IO.Put_Line("     Job successfully imported.");
    end Add;
    
    function Get_By_Id(Id : Natural) return Job_Entry_Record_Access is
      Cursor : Job_Entry_Record_Vectors.Cursor := Jobs.First;
    begin
      loop
        exit when Job_Entry_Record_Vectors."="(Cursor, Job_Entry_Record_Vectors.No_Element);
        
        declare 
          Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
        begin
          if Get_Job_Id(Element.Job) = Id then
            return Element;
          end if;
        end;
        
        Job_Entry_Record_Vectors.Next(Cursor);
      end loop;
      
      return null;
    end Get_By_Id;
    
    function Get_Next_Pending return Job_Entry_Record_Access is
      Cursor : Job_Entry_Record_Vectors.Cursor := Jobs.First;
    begin
      loop
        exit when Job_Entry_Record_Vectors."="(Cursor, Job_Entry_Record_Vectors.No_Element);
        
        declare 
          Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
        begin
          if Master_Helper."="(Element.State, Master_Helper.Pending) then
            Element.State := Master_Helper.In_Progress;
            return Element;
          end if;
        end;
        
        Job_Entry_Record_Vectors.Next(Cursor);
      end loop;
      
      Ada.Exceptions.Raise_Exception(Master_Helper.No_Job_Found'Identity, "No futher job found.");
    end Get_Next_Pending;
    
    function Count return Natural is
    begin
      return Natural(Jobs.Length);
    end Count;
    
    function Count_By_State(State : Master_Helper.Job_State) return Natural is
      use Master_Helper;
      
      Counter : Natural := 0;
      
      procedure Count(Cursor : Job_Entry_Record_Vectors.Cursor) is
        Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
      begin
        if Element.State = State then
          Counter := Counter + 1;
        end if;
      end Count;
    
    begin
      Jobs.Iterate(Count'Access);
      
      return Counter;
    end Count_By_State;
    
    procedure Print is
      
      procedure Print(Cursor : Job_Entry_Record_Vectors.Cursor) is
        Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
      begin
        Print_Job(Element.Job, Master_Helper.To_String(Element.State));
      end Print;
      
    begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Jobs:");
      Ada.Text_IO.New_Line;
      
      Jobs.Iterate(Print'Access);
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Print;
  end Jobs;
  
  
  procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State) is
  begin
    Job_Entry.State := State;
  end Change_Job_State;
  
  
  function Job_Entry_To_Xml(Job_Entry : Job_Entry_Record_Access) return String is
  begin
    return To_Xml(Job_Entry.Job);
  end Job_Entry_To_Xml;
  
  function Job_Is_Null(Job_Entry : Job_Entry_Record_Access) return Boolean is
  begin
    return Job_Entry = Null;
  end Job_Is_Null;
  
  
  protected body Worker is
  
    procedure Add(New_Worker : Master_Helper.Worker_Record_Access) is
      subtype Rand_Range is Integer range 1..999999;
      package Rand is new Ada.Numerics.Discrete_Random(Rand_Range);
      Gen : Rand.Generator;
    begin
      Rand.Reset(Gen);
      
      --TODO: HashFunktion in Paket auslagern.
      --TODO: Salt für Hashfunktion in Xml_Datei
      --TODO: Method Authentification Code (HMAC)
      New_Worker.Access_Token := GNAT.MD5.Digest(
        ASU.To_String(New_Worker.Identifier) & "-" & Master_Helper.To_String(New_Worker.W_Type) & "-" & Rand.Random(Gen)'Img
      );
      
      Worker.Append(New_Worker);
    end Add;
    
    function Find_By_Access_Token_And_Type(Access_Token : String; W_Type : Master_Helper.Worker_Type) return Master_Helper.Worker_Record_Access is
      Cursor : Master_Helper.Worker_Entry_Vectors.Cursor := Worker.First;
    begin
      loop
        exit when Master_Helper.Worker_Entry_Vectors."="(Cursor, Master_Helper.Worker_Entry_Vectors.No_Element);
        
        declare
          Worker : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(Cursor);
        begin
          
          if Master_Helper."="(Worker.W_Type, W_Type) and Worker.Access_Token = Access_Token then
            return Worker;
          end if;
        end;
        
        Master_Helper.Worker_Entry_Vectors.Next(Cursor);
        
      end loop;
      
      Ada.Exceptions.Raise_Exception(Master_Helper.No_Worker_Found'Identity, "No worker found");
      
    end Find_By_Access_Token_And_Type;
    
    procedure Print is
      
      procedure Print(Cursor : Master_Helper.Worker_Entry_Vectors.Cursor) is
        Worker_Entry : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(Cursor);
      begin
        Utility.Put(ASU.To_String(Worker_Entry.Identifier), 30, 2);
        Utility.Put(Master_Helper.To_String(Worker_Entry.W_Type), 10, 2);
        Utility.Put(Worker_Entry.Access_Token, 40, 2);
        Utility.Put(GNAT.Sockets.Image(Worker_Entry.Ip), 20, 2);
        Utility.Put(Worker_Entry.Port'Img, 20, 2);
        Ada.Text_IO.New_Line;
      end Print;
      
    begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Connected worker:");
      Ada.Text_IO.New_Line;
      Utility.Put("Identifier", 30, 2);
      Utility.Put("Type", 10, 2);
      Utility.Put("Access Token", 40, 2);
      Utility.Put("IP address", 20, 2);
      Utility.Put("Listen on port", 20, 2);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("------------------------------------------------------------------------------------------------------------------");
      
      Worker.Iterate(Print'Access);
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Print;
  
  end Worker;
  
  
  
  ----------------------------------------------------
  -- GENERIC CONSOLE METHODS                        --
  ----------------------------------------------------
  function Banner return String is
  begin
    return "ADA MR Master";
  end Banner;
  
  procedure Parse_Configuration(Config_Xml : Xml.Node_Access) is
  begin
    Master_Helper.Server_Bind_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Config_Xml, "bind_ip"));
    Master_Helper.Server_Bind_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Config_Xml, "bind_port"));
    
    declare
      Reducer_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "reducer");
    begin
      Master_Helper.Reducer_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Reducer_Details, "ip"));
      Master_Helper.Reducer_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Reducer_Details, "port"));
    end;
  end Parse_Configuration;
  
  procedure Process_User_Input(User_Input : String; To_Controll : Master_Task_Access) is
  begin
    if (Is_Equal(User_Input, "start", true)) then
      To_Controll.Start(To_Controll);
    
    elsif (Is_Equal(User_Input, "help", true)) then
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("  Commands:");
      Ada.Text_IO.Put_Line("    start        Starts the Ada MR Master Server");
      Ada.Text_IO.Put_Line("    worker       Prints all connected worker");
      Ada.Text_IO.Put_Line("    quit         Exit Ada MR Master and stop all mapper and reducer");
      Ada.Text_IO.Put_Line("    verbose-on   Enable verbose mode to display log entries");
      Ada.Text_IO.Put_Line("    verbose-off  Disable verbose mode");
      Ada.Text_IO.Put_Line("    jobs         Number of unprocessed jobs");
      Ada.Text_IO.Put_Line("    help         Displays this message");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    
    elsif (Utility.Is_Equal(User_Input, "config", true)) then
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("-> " & Banner & " configuration");
            
      Utility.Put("IP address:", 20, 2);
      Utility.Put(GNAT.Sockets.Image(Master_Helper.Server_Bind_Ip), 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Port:", 20, 2);
      Utility.Put(Master_Helper.Server_Bind_Port'Img, 60, 2);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    
    elsif Utility.Is_Equal(User_Input, "quit", true) OR Is_Equal(User_Input, "exit", true) then
      To_Controll.Stop;
    
    elsif (Is_Equal(User_Input, "verbose-on", true)) then
      Logger.Enable_Verbose_Mode;
      Ada.Text_IO.Put_Line("Verbose mode: On");
    
    elsif (Is_Equal(User_Input, "verbose-off", true)) then
      Logger.Disable_Verbose_Mode;
      Ada.Text_IO.Put_Line("Verbose mode: Off");
    
    elsif (Is_Equal(User_Input, "worker", true)) then
      Worker.Print;
    
    elsif (Is_Equal(User_Input, "jobs", true)) then
      Jobs.Print;
    
    else
      Ada.Text_IO.Put_Line("Unknown command: " & User_Input);
    end if;
  end Process_User_Input;
  
  
  
  
  
  
end Master;