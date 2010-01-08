with Ada.Text_IO;
with Application_Helper;
with Logger;

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
          Logger.Put_Line("Observer task started", Logger.Info);
          
          loop
            exit when Exit_Observer = true;
            exit when Observe(To_Controll) = true;
          end loop;
          
        exception
          when Error : others => Application_Helper.Print_Exception(Error, "Observer Task");
        end;
      or
        accept Stop;
        Logger.Put_Line("Terminating observer task", Logger.Info);
        exit;
      end select;
    end loop;
    
    Logger.Put_Line("Observer task terminated", Logger.Info);
    
  end Observer_Task;
  
end Generic_Observer;