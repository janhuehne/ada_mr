with Ada.Text_IO;
with Ada_Mr.Helper;
with Ada_Mr.Logger;

package body Ada_Mr.Generics.Observer is
  
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
          Ada_Mr.Logger.Put_Line("Observer task started", Ada_Mr.Logger.Info);
          
          loop
            exit when Exit_Observer = true;
            exit when Observe(To_Controll) = true;
          end loop;
          
        exception
          when Error : others => Ada_Mr.Helper.Print_Exception(Error, "Observer Task");
        end;
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line("Terminating observer task", Ada_Mr.Logger.Info);
        exit;
      end select;
    end loop;
    
    Ada_Mr.Logger.Put_Line("Observer task terminated", Ada_Mr.Logger.Info);
  end Observer_Task;
  
end Ada_Mr.Generics.Observer;