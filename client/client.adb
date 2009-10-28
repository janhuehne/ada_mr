with Ada.Text_IO;


with Utility;
use Utility;

with Logger;

package body Client is
  
  task body Client_Task is
  begin
    loop
      select
        accept Say_Hello;
        Ada.Text_IO.Put_Line("Hello!");
      end select;
    end loop;
  end Client_Task;
  
  
  task body Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    C               : Client_Task_Access;
  begin
    accept Start(C_Arg : Client_Task_Access) do
      C := C_Arg;
    end Start;
    
    Ada.Text_IO.Put_Line("Client Console");
    
    Ada.Text_IO.Put(":> ");
    
    loop
--      Ada.Text_IO.Get_Line(In_String, In_Last);
--      
--      if In_Last > 0 then
--        if (Is_Equal(In_String, In_Last, "start", true)) then
--          M.Start_Master(M);
--          
--        elsif (Is_Equal(In_String, In_Last, "quit", true)) then
--          M.Stop_Master;
--          exit; 
--          
--        elsif (Is_Equal(In_String, In_Last, "help", true)) then
--          Ada.Text_IO.Put_Line("");
--          Ada.Text_IO.Put_Line("  Commands:");
--          Ada.Text_IO.Put_Line("    start        Starts the Ada MR Master Server");
--          Ada.Text_IO.Put_Line("    idle-mappers Prints the idle workers");
--          Ada.Text_IO.Put_Line("    quit         Exit Ada MR Server Server");
--          Ada.Text_IO.Put_Line("    verbose-on   Enable verbose mode to display log entries");
--          Ada.Text_IO.Put_Line("    verbose-off  Disable verbose mode");
--          Ada.Text_IO.Put_Line("    jobs         Number of unprocessed jobs");
--          Ada.Text_IO.Put_Line("    help         Displays this message");
--          Ada.Text_IO.New_Line;
--          Ada.Text_IO.New_Line;
--          Ada.Text_IO.New_Line;
--          
--        elsif (Is_Equal(In_String, In_Last, "verbose-on", true)) then
--          Logger.Enable_Verbose_Mode;
--          Ada.Text_IO.Put_Line("Verbose mode: On");
--        
--        elsif (Is_Equal(In_String, In_Last, "verbose-off", true)) then
--          Logger.Disable_Verbose_Mode;
--          Ada.Text_IO.Put_Line("Verbose mode: Off");
--        elsif (Is_Equal(In_String, In_Last, "idle-mappers", true)) then
--          Worker.Print_All_Idle_Mapper;
--        elsif (Is_Equal(In_String, In_Last, "jobs", true)) then
----          Ada.Text_IO.Put_Line(Unprocessed_Jobs.Length'Img & " unprocessed jobs");
----          Xml_Queue.Print_Jobs;
--            Print_Jobs;
--        else
--          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
--        end if;
--      end if;
--        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Console;
    
end Client;