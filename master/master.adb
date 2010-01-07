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
with Xml_Parser;

package body Master is
  
  task body Master_Task is
    Master_Server_Task : Server.Server.Server_Task;
    Observer_Task      : Observer.Observer_Task;
    Console_Task       : Console.Console;
    
    procedure Read_and_Parse_Config_File(Config_File : String) is
    begin
      if Utility.Does_File_Exist(Config_File) then
        Ada.Text_IO.Put_Line("Parsing config file");
        Parse_Configuration(
          Xml_Parser.Parse(File_Name => Config_File)
        );
        Ada.Text_IO.Put_Line("--> Done");
      else
        Ada.Text_IO.Put_Line("No config file found!");
      end if;
    exception
      when Error : others => 
        Utility.Print_Exception(Error);
        Ada.Exceptions.Raise_Exception(Utility.Configuration_File_Error'Identity, "There is a problem with the configuration file.");
    end Read_and_Parse_Config_File;
    
  begin
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("          _____               __  __ _____      __  __           _            ");
    Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  \/  |         | |           ");
    Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | \  / | __ _ ___| |_ ___ _ __ ");
    Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    | |\/| |/ _` / __| __/ _ \ '__|");
    Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | |  | | (_| \__ \ |_  __/ |   ");
    Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  |_|\__,_|___/\__\___|_|   ");

    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    
    loop
      select
        accept Start(Self : Master_Task_Access; Config_File : String) do
          Main_Task := Self;
          
          -- parse configuration
          Read_and_Parse_Config_File(Config_File);
        end Start;
        
        Split_Raw_Data;
        
        
        Logger.Put_Line("Importing jobs ...", Logger.Info);
        
        loop
          declare
          begin
            Jobs.Add(Get_Next_Raw_Job);
          exception
            when CONSTRAINT_ERROR => exit;
          end;
        end loop;
        
        Logger.Put_Line(Jobs.Count'Img & " jobs imported", Logger.Info);
        
        Console_Task.Start(
          Main_Task
        );
        
        Master_Server_Task.Start(
          Master_Helper.Server_Bind_Ip, 
          Master_Helper.Server_Bind_Port
        );
        
        Observer_Task.Start(Main_Task);
      or
        accept Stop;
        Logger.Put_Line(" -> Please wait, while closing the client connections.", Logger.Info);
        Master_Helper.Aborted.Set_Exit;
        Master_Server_Task.Stop;
        Observer_Task.Stop;
        exit;
      end select;
    end loop;
  end Master_Task;
  
  procedure Stop_Master_Task is
  begin
    Main_Task.Stop;
  end Stop_Master_Task;
  
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
    
      Logger.Put_Line("All jobs done", Logger.Info);
      
      -- TODO: send this to all connected reducers!
      declare
        Reducer_Vector : Master_Helper.Worker_Entry_Vectors.Vector := Worker.Find_All_By_Type(Utility.Reducer);
        
        procedure Send_Finalize(C : Master_Helper.Worker_Entry_Vectors.Cursor) is
          Reducer : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(C);
        begin
          declare
            Response : String := Utility.Send(
              Reducer.Ip,
              Reducer.Port,
              Xml_Helper.Xml_Command(Xml_Helper.Master, "finalize"),
              5
            );
          begin
            null;
          end;
        end Send_Finalize;
      begin
        Reducer_Vector.Iterate(Send_Finalize'Access);
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
      Logger.Put_Line("--> Job successfully imported.", Logger.Info);
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
  
  
  procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State; Message : String := "") is
  begin
    Job_Entry.State := State;
    
    if Message /= "" then
      Job_Entry.Message := ASU.To_Unbounded_String(Message);
    end if;
    
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
        ASU.To_String(New_Worker.Identifier) & "-" & Utility.To_String(New_Worker.W_Type) & "-" & Rand.Random(Gen)'Img
      );
      
      Worker.Append(New_Worker);
    end Add;
    
    function Find_By_Access_Token_And_Type(Access_Token : String; W_Type : Utility.Worker_Type) return Master_Helper.Worker_Record_Access is
      Cursor : Master_Helper.Worker_Entry_Vectors.Cursor := Worker.First;
    begin
      loop
        exit when Master_Helper.Worker_Entry_Vectors."="(Cursor, Master_Helper.Worker_Entry_Vectors.No_Element);
        
        declare
          Worker : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(Cursor);
        begin
          
          if Utility."="(Worker.W_Type, W_Type) and Worker.Access_Token = Access_Token then
            return Worker;
          end if;
        end;
        
        Master_Helper.Worker_Entry_Vectors.Next(Cursor);
        
      end loop;
      
      Ada.Exceptions.Raise_Exception(Master_Helper.No_Worker_Found'Identity, "No worker found");
      
    end Find_By_Access_Token_And_Type;
    
    
    function Find_All_By_Type(W_Type : Utility.Worker_Type) return Master_Helper.Worker_Entry_Vectors.Vector is
      Type_Vector : Master_Helper.Worker_Entry_Vectors.Vector;
      
      procedure Find(C : Master_Helper.Worker_Entry_Vectors.Cursor) is
        Worker : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(C);
      begin
        if Utility."="(Worker.W_Type, W_Type) then
          Type_Vector.Append(Worker);
        end if;
      end Find;
    begin
      Worker.Iterate(Find'Access);
      
      return Type_Vector;
    end Find_All_By_Type;
    
    procedure Print is
      
      procedure Print(Cursor : Master_Helper.Worker_Entry_Vectors.Cursor) is
        Worker_Entry : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(Cursor);
      begin
        Utility.Put(ASU.To_String(Worker_Entry.Identifier), 30, 2);
        Utility.Put(Utility.To_String(Worker_Entry.W_Type), 10, 2);
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
  end Parse_Configuration;
  
  procedure Process_User_Input(User_Input : String; To_Controll : Master_Task_Access) is
  begin
    if (Is_Equal(User_Input, "start", true)) then
--      To_Controll.Start(To_Controll);
      null;
    
    elsif (Is_Equal(User_Input, "help", true)) then
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("  Commands:");
      Ada.Text_IO.Put_Line("    start        Starts the Ada MR Master Server");
      Ada.Text_IO.Put_Line("    worker       Prints all connected worker");
      Ada.Text_IO.Put_Line("    quit         Exit Ada MR Master and stop all mapper and reducer");
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
    
    elsif (Is_Equal(User_Input, "worker", true)) then
      Worker.Print;
    
    elsif (Is_Equal(User_Input, "jobs", true)) then
      Jobs.Print;
    
    else
      Ada.Text_IO.Put_Line("Unknown command: " & User_Input);
    end if;
  end Process_User_Input;
  
  
  
  
  
  
end Master;