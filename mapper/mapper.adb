with Ada.Text_IO;


with Utility;
use Utility;

with Logger;

with Runner;

package body Mapper is
  
  task body Mapper_Task is
    R : Runner.Runner_Task;
  begin
    loop
      select
        accept Start;
        Ada.Text_IO.Put_Line("Hier muss was gestartet werden!");
        R.Start;
      or
        accept Stop;
        Runner.Aborted.Stop;
        exit;
      end select;
    end loop;
  end Mapper_Task;
  
  
  task body Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    C               : Mapper_Task_Access;
  begin
    accept Start(C_Arg : Mapper_Task_Access) do
      C := C_Arg;
    end Start;
    
    Ada.Text_IO.Put_Line("Client Console");
    
    Ada.Text_IO.Put(":> ");
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        
        if (Is_Equal(In_String, In_Last, "config", true)) then
          Ada.Text_IO.Put_Line("Show Config!");
          
        elsif (Is_Equal(In_String, In_Last, "start", true)) then
          C.Start;
        
        elsif (Is_Equal(In_String, In_Last, "quit", true)) or (Is_Equal(In_String, In_Last, "exit", true)) then
          C.Stop;
          exit; 
        
        elsif (Is_Equal(In_String, In_Last, "help", true)) then
          Ada.Text_IO.Put_Line("");
          Ada.Text_IO.Put_Line("  Commands:");
          Ada.Text_IO.Put_Line("    start        Starts the Ada MR Client");
          Ada.Text_IO.Put_Line("    abort        Stops the Ada MR Client immediately (current job is aborted)");
          Ada.Text_IO.Put_Line("    quit / exit  Stops the Ada MR Client after processing the last job");
        
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
        else
          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
        end if;
      end if;
--        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Console;
    
end Mapper;