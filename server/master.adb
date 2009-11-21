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
    Master_Server_Task : Master_Server.Server_Task;
    Me                 : Master_Task_Access;
  begin
    loop
      select
        accept Start_Master(M : Master_Task_Access) do
          Me := M;
        end Start_Master;
        
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
        
        Master_Server_Task.Start;
      or
        accept Stop_Master;
        Ada.Text_IO.Put_Line("Please wait, while closing the client connections.");
        Master_Helper.Aborted.Stop_Master;
        Master_Server_Task.Stop;
        exit;
      end select;
    end loop;
  end Master_Task;
  
  task body Observe_Jobs is
    use GNAT.Sockets;
    Reducer_IP : String := "127.0.0.1";
    Reducer_Port : String := "7100";
  begin
    accept Start;
    
    Ada.Text_IO.Put_Line("Observing jobs ...");
    
    loop
     exit when Master_Helper.Aborted.Check_Clients = true;
      
--       if Xml_Queue.Jobs.Is_Empty = false and Xml_Queue.All_Jobs_Done = true then
--         
--         Ada.Text_IO.Put_Line("All jobs done. Sending message to the reducer!!!!");
--         
--         -- Send all jobs done message to the reducers!
--         declare
--           Sock            : Socket_Type;
--           S               : Stream_Access;
--           Addr            : Sock_Addr_Type (Family_Inet);
-- --          Msg             : String (1 .. 2000);
-- --          Last            : Natural;
--           B               : Boolean;
--           Read_Selector   : Selector_Type;
--           Read_Set, WSet  : Socket_Set_Type;
--           Read_Status     : Selector_Status;
--         begin
--           Create_Socket(Sock);
--           Addr.Addr := Addresses(Get_Host_By_Name (Reducer_IP), 1);
--           Addr.Port := Port_Type'Value(Reducer_Port);
--           
--           Create_Selector(Read_Selector);
--           Empty(Read_Set);
--           Empty(WSet);
--           
--           Connect_Socket(Sock, Addr);
--           S := Stream (Sock);
--           Boolean'Read (S, B);
--           
--           Set(Read_Set, Sock);
--           
--           -- check for input on socket (server may be aborting)
--           -- time-out immediately if no input pending
--           -- We seem to need a small delay here (using zero seems to block
--           -- forever)
--           -- Is this a GNAT bug or AB misreading Check_Selector docs?
--           Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
--           
--           String'Output(
--             S,
--             Xml_Helper.Xml_Command(Xml_Helper.Master, "finalize")
--           );
--           
--           declare
--             Str : String := String'Input(S);
--           begin
--             Ada.Text_IO.Put_Line(Str);
--           end;
--           
--           ShutDown_Socket(Sock);
--           Close_Selector(Read_Selector);
--           
--           exit;
--         exception 
--           when others => exit;
--         end;
--       end if;
      
    end loop;
    
  end Observe_Jobs;
  
  
  task body Master_Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    M               : Master_Task_Access;
  begin
    accept Start(M_Arg : Master_Task_Access) do
      M := M_Arg;
    end Start;
    
    Ada.Text_IO.Put_Line("Master Console");
    
    Ada.Text_IO.Put(":> ");
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        if (Is_Equal(In_String, In_Last, "start", true)) then
          M.Start_Master(M);
          
        elsif (Is_Equal(In_String, In_Last, "quit", true)) then
          M.Stop_Master;
          exit; 
          
        elsif (Is_Equal(In_String, In_Last, "help", true)) then
          Ada.Text_IO.Put_Line("");
          Ada.Text_IO.Put_Line("  Commands:");
          Ada.Text_IO.Put_Line("    start        Starts the Ada MR Master Server");
          Ada.Text_IO.Put_Line("    worker       Prints all connected worker");
          Ada.Text_IO.Put_Line("    quit         Exit Ada MR Server Server");
          Ada.Text_IO.Put_Line("    verbose-on   Enable verbose mode to display log entries");
          Ada.Text_IO.Put_Line("    verbose-off  Disable verbose mode");
          Ada.Text_IO.Put_Line("    jobs         Number of unprocessed jobs");
          Ada.Text_IO.Put_Line("    help         Displays this message");
          Ada.Text_IO.New_Line;
          Ada.Text_IO.New_Line;
          Ada.Text_IO.New_Line;
          
        elsif (Is_Equal(In_String, In_Last, "verbose-on", true)) then
          Logger.Enable_Verbose_Mode;
          Ada.Text_IO.Put_Line("Verbose mode: On");
        
        elsif (Is_Equal(In_String, In_Last, "verbose-off", true)) then
          Logger.Disable_Verbose_Mode;
          Ada.Text_IO.Put_Line("Verbose mode: Off");
        elsif (Is_Equal(In_String, In_Last, "worker", true)) then
          Worker.Print;
--          Worker.Print_All_Idle_Mapper;
        elsif (Is_Equal(In_String, In_Last, "jobs", true)) then
          Jobs.Print;
--          Ada.Text_IO.Put_Line(Unprocessed_Jobs.Length'Img & " unprocessed jobs");
--          Xml_Queue.Print_Jobs;
            Print_Jobs;
        else
          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
        end if;
      end if;
        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Master_Console;
  
  
--  function Get_Next_Job(Remove_From_Vector : Boolean := true) return My_Job is
--    Job_Cursor : Job_Vector.Cursor := Unprocessed_Jobs.First;
--    Job        : My_Job            := Unprocessed_Jobs.Element(Job_Vector.To_Index(Job_Cursor));
--  begin
--    if Remove_From_Vector = true then
--      Unprocessed_Jobs.Delete(Job_Vector.To_Index(Job_Cursor));
--    end if;
--    
--    return Job;
--  end Get_Next_Job;
  
  procedure Print_Jobs is
    
--    procedure Print(Position : Job_Vector.Cursor) is
--      Job : My_Job := Unprocessed_Jobs.Element(Job_Vector.To_Index(Position));
--    begin
--      Print_Job(
--        Job,
--        Xml_Queue.Get_Job_State(Get_Job_Id(Job))
--      );
--    end Print;
    
  begin
--    Unprocessed_Jobs.Iterate(Print'Access);
    null;
  end Print_Jobs;


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
  
  
  function Job_To_Xml(Job_Entry : Job_Entry_Record_Access) return String is
  begin
    return To_Xml(Job_Entry.Job);
  end Job_To_Xml;
  
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
      
      New_Worker.Access_Token := GNAT.MD5.Digest(
        ASU.To_String(New_Worker.Identifier) & "-" & Master_Helper.To_String(New_Worker.W_Type) & "-" & Rand.Random(Gen)'Img
      );
      
      Worker.Append(New_Worker);
    end Add;
    
    procedure Print is
      
      procedure Print(Cursor : Master_Helper.Worker_Entry_Vectors.Cursor) is
        Worker_Entry : Master_Helper.Worker_Record_Access := Master_Helper.Worker_Entry_Vectors.Element(Cursor);
      begin
        Utility.Put(ASU.To_String(Worker_Entry.Identifier), 30, 2);
        Utility.Put(Master_Helper.To_String(Worker_Entry.W_Type), 10, 2);
        Utility.Put(Worker_Entry.Access_Token, 40, 2);
        Utility.Put(GNAT.Sockets.Image(Worker_Entry.Ip), 20, 2);
        Utility.Put(Worker_Entry.Listen_Port'Img, 20, 2);
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
  
end Master;