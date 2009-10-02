with Ada.Text_IO;


with Utility;
use Utility;

with Server;
with Logger;

with Worker;

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
          Ada.Text_IO.Put_Line("    help         Displays this message");
          Ada.Text_IO.Put_Line("   ");
          Ada.Text_IO.Put_Line("");
          Ada.Text_IO.Put_Line("");
          
        elsif (Is_Equal(In_String, In_Last, "verbose-on", true)) then
          Logger.Enable_Verbose_Mode;
          Ada.Text_IO.Put_Line("Verbose mode: On");
        
        elsif (Is_Equal(In_String, In_Last, "verbose-off", true)) then
          Logger.Disable_Verbose_Mode;
          Ada.Text_IO.Put_Line("Verbose mode: Off");
        elsif (Is_Equal(In_String, In_Last, "idle-mappers", true)) then
          Worker.Print_All_Idle_Mapper;
        else
          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
        end if;
      end if;
        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Master_Console;
  
end Master;