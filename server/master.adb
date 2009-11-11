with Ada.Text_IO;


with Utility;
use Utility;

with Server;
with Logger;

with Worker;
with Xml_Queue;
with Xml_Helper;
with GNAT.Sockets;

package body Master is
  
  task body Master_Task is
    Main_Server : Server.P_Server;
    Me          : Master_Task_Access;
  begin
    loop
      select
        accept Import_Jobs(Jobs : Job_Vector.Vector) do
          Unprocessed_Jobs := Jobs;
        end Import_Jobs;
        
        Ada.Text_IO.Put_Line(Unprocessed_Jobs.Length'Img);
      or
        accept Start_Master(M : Master_Task_Access) do
          Me := M;
        end Start_Master;
        
        Ada.Text_IO.Put_Line("Starting Master Service");
        
        declare
          Cursor : Job_Vector.Cursor := Unprocessed_Jobs.First;
        begin
          Ada.Text_IO.Put_Line("  .. Preparing job containers");
          loop
            Xml_Queue.Add_Job(Get_Job_Id(Job_Vector.Element(Cursor)), To_Xml(Job_Vector.Element(Cursor)));
            Job_Vector.Next(Cursor);
            
            exit when Job_Vector."="(Cursor, Job_Vector.No_Element);
          end loop;
          Ada.Text_IO.Put_Line("      -> " & Xml_Queue.Count_Jobs(Xml_Queue.Pending)'Img & " jobs imported.");
        end;
        
        Main_Server.Start;
      or
        accept Split_Data;
        Ada.Text_IO.Put_Line("Split Data!"); 
      or
        accept Add_Job;
        Ada.Text_IO.Put_Line("Calling add job!"); 
      or
        accept Stop_Master;
        Ada.Text_IO.Put_Line("Please wait, while closing the client connections.");
        Server.Aborted.Stop_Master;
        Main_Server.Stop;
        exit;
      or
        accept Say_Hello;
        Ada.Text_IO.Put_Line("Hello!");
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
     exit when Server.Aborted.Check_Clients = true;
      
      if Xml_Queue.Jobs.Is_Empty = false and Xml_Queue.All_Jobs_Done = true then
        
        Ada.Text_IO.Put_Line("All jobs done. Sending message to the reducer!!!!");
        
        -- Send all jobs done message to the reducers!
        declare
          Sock            : Socket_Type;
          S               : Stream_Access;
          Addr            : Sock_Addr_Type (Family_Inet);
--          Msg             : String (1 .. 2000);
--          Last            : Natural;
          B               : Boolean;
          Read_Selector   : Selector_Type;
          Read_Set, WSet  : Socket_Set_Type;
          Read_Status     : Selector_Status;
        begin
          Create_Socket(Sock);
          Addr.Addr := Addresses(Get_Host_By_Name (Reducer_IP), 1);
          Addr.Port := Port_Type'Value(Reducer_Port);
          
          Create_Selector(Read_Selector);
          Empty(Read_Set);
          Empty(WSet);
          
          Connect_Socket(Sock, Addr);
          S := Stream (Sock);
          Boolean'Read (S, B);
          
          Set(Read_Set, Sock);
          
          -- check for input on socket (server may be aborting)
          -- time-out immediately if no input pending
          -- We seem to need a small delay here (using zero seems to block
          -- forever)
          -- Is this a GNAT bug or AB misreading Check_Selector docs?
          Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
          
          String'Output(
            S,
            Xml_Helper.Xml_Command(Xml_Helper.Master, "finalize")
          );
          
          declare
            Str : String := String'Input(S);
          begin
            Ada.Text_IO.Put_Line(Str);
          end;
          
          ShutDown_Socket(Sock);
          Close_Selector(Read_Selector);
          
          exit;
        exception 
          when others => exit;
        end;
      end if;
      
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
          Ada.Text_IO.Put_Line("    idle-mappers Prints the idle workers");
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
        elsif (Is_Equal(In_String, In_Last, "idle-mappers", true)) then
          Worker.Print_All_Idle_Mapper;
        elsif (Is_Equal(In_String, In_Last, "jobs", true)) then
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
  
  procedure Add_New_Job(Job : My_Job) is
  begin
    Unprocessed_Jobs.Append(Job);
  end Add_New_Job;
  
  function Get_Next_Job(Remove_From_Vector : Boolean := true) return My_Job is
    Job_Cursor : Job_Vector.Cursor := Unprocessed_Jobs.First;
    Job        : My_Job            := Unprocessed_Jobs.Element(Job_Vector.To_Index(Job_Cursor));
  begin
    if Remove_From_Vector = true then
      Unprocessed_Jobs.Delete(Job_Vector.To_Index(Job_Cursor));
    end if;
    
    return Job;
  end Get_Next_Job;
  
  procedure Print_Jobs is
    
    procedure Print(Position : Job_Vector.Cursor) is
      Job : My_Job := Unprocessed_Jobs.Element(Job_Vector.To_Index(Position));
    begin
      Print_Job(
        Job,
        Xml_Queue.Get_Job_State(Get_Job_Id(Job))
      );
    end Print;
    
  begin
    Unprocessed_Jobs.Iterate(Print'Access);
  end Print_Jobs;

  
end Master;