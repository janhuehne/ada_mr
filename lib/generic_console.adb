with Ada.Text_IO;
with Ada.Exceptions;
with Application_Helper;
with Logger;

package body Generic_Console is
  
  task body Console is
    In_String           : String(1..20);
    In_Last             : Natural;
    To_Controll         : To_Controll_Task_Access;
  begin
    accept Start(M_Arg : To_Controll_Task_Access) do
      To_Controll         := M_Arg;
    end Start;
    
    Logger.Put_Line("Welcome to " & Banner & " console!", Logger.Info);
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        declare
          User_Input : String := In_String(1 .. In_Last);
        begin
          Process_User_Input(User_Input, To_Controll);
          
          if Application_Helper.Is_Equal(User_Input, "quit", true) OR 
             Application_Helper.Is_Equal(User_Input, "exit", true) OR 
             Application_Helper.Is_Equal(User_Input, "abort", true)
          then
            exit;
          end if;
          
        exception
          when Error : others => Application_Helper.Print_Exception(Error);
        end;
      end if;
        
      Ada.Text_IO.Put(":> ");

    end loop;
    
  end Console;
  
end Generic_Console;