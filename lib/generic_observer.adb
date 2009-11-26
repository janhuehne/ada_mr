with Ada.Text_IO;
with Utility;

package body Generic_Observer is
  
  task body Observer_Task is
    To_Controll : To_Controll_Task_Access;
  begin
    loop
      select
        accept Start(Arg : To_Controll_Task_Access) do
          To_Controll := Arg;
        end;
        
        declare
        begin
          Ada.Text_IO.Put_Line("-> Observer task started");
          
          loop
            exit when Exit_Observer = true;
            exit when Observe(To_Controll) = true;
          end loop;
          
        exception
          when Error : others => Utility.Print_Exception(Error, "Observer Task");
        end;
      or
        accept Stop;
        Ada.Text_IO.Put_Line("-> Stopping server task");
        exit;
      end select;
    end loop;
    
    Ada.Text_IO.Put_Line("-> Observer task terminated");
    
  end Observer_Task;
  
end Generic_Observer;