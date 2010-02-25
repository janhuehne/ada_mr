with Ada.Text_IO;
with Ada.Exceptions;
with Ada_Mr.Helper;
with Ada_Mr.Logger;

package body Ada_Mr.Generics.Console is
  
  -------------
  -- Console --
  -------------
  task body Console is
    In_String           : String(1..20);
    In_Last             : Natural;
    To_Controll         : To_Controll_Task_Access;
  begin
    accept Start
      (M_Arg : To_Controll_Task_Access)
    do
      To_Controll := M_Arg;
    end Start;
    
    Ada_Mr.Logger.Put_Line("Welcome to " & Banner & " console!", Ada_Mr.Logger.System);
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        declare
          User_Input : String := In_String(1 .. In_Last);
        begin
          Process_User_Input(User_Input, To_Controll);
          
          if Ada_Mr.Helper.Is_Equal(User_Input, "quit", true) OR 
             Ada_Mr.Helper.Is_Equal(User_Input, "exit", true) OR 
             Ada_Mr.Helper.Is_Equal(User_Input, "abort", true)
          then
            exit;
          end if;
          
        exception
          when Error : others => Ada_Mr.Helper.Print_Exception(Error);
        end;
      end if;
        
      Ada.Text_IO.Put(":> ");
    end loop;
  end Console;
  
end Ada_Mr.Generics.Console;