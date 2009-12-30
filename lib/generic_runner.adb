with Logger;
with Utility;

package body Generic_Runner is

  task body Runner_Task is 
  begin 
    loop
      select
        accept Start;
        Logger.Put_Line("Runner task started!", Logger.Info);
        declare
        begin
          Run;
        exception
          when Error : others => Utility.Print_Exception(Error);
        end;
      or
        accept Stop;
        Logger.Put_Line("Terminating runner task", Logger.Info);
        exit;
      end select;
    end loop;
    
    Logger.Put_Line("Runner task terminated", Logger.Info);
  end Runner_Task;
  
end Generic_Runner;

