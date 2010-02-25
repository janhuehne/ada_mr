with Ada_Mr.Logger;
with Ada_Mr.Helper;

package body Ada_Mr.Generics.Runner is
  -----------------
  -- Runner_Task --
  -----------------
  task body Runner_Task is 
  begin 
    loop
      select
        accept Start;
        Ada_Mr.Logger.Put_Line("Runner task started!", Ada_Mr.Logger.System);
        declare
        begin
          Run;
        exception
          when Error : others => Ada_Mr.Helper.Print_Exception(Error);
        end;
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line("Terminating runner task", Ada_Mr.Logger.System);
        exit;
      end select;
    end loop;
    
    Ada_Mr.Logger.Put_Line("Runner task terminated", Ada_Mr.Logger.System);
  end Runner_Task;
  
end Ada_Mr.Generics.Runner;

