with Ada.Text_IO;

with Utility;
use Utility;

with Logger;
with Reducer_Helper;

package body Reducer is
  
  task body Reducer_Task is
    R : Runner_MR.Runner_Task;
  begin
    loop
      select
        accept Start;
        Ada.Text_IO.Put_Line("Hier muss was gestartet werden!");
        R.Start;
      or
        accept Stop;
        Reducer_Helper.Aborted.Stop;
        R.Stop;
        exit;
      end select;
    end loop;
    null;
  end Reducer_Task;
  
  
  task body Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    C               : Reducer_Task_Access;
  begin
    accept Start(C_Arg : Reducer_Task_Access) do
      C := C_Arg;
    end Start;
    
    Ada.Text_IO.Put_Line("Reducer Console");
    
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
          Ada.Text_IO.Put_Line("    start        Starts the Ada MR Reducer");
          Ada.Text_IO.Put_Line("    abort        Stops the Ada MR Reducer immediately (current job is aborted)");
          Ada.Text_IO.Put_Line("    quit / exit  Stops the Ada MR Reducer after processing the last job");
        
        else
          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
        end if;
      end if;
--        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Console;
    
end Reducer;