with Ada.Text_IO;
with Ada.Exceptions;
with Utility;

package body Generic_Console is
  
  task body Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    To_Controll     : To_Controll_Task_Access;
    Config          : Xml.Node_Access;
  begin
    accept Start(M_Arg : To_Controll_Task_Access; Config_Xml : Xml.Node_Access) do
      To_Controll := M_Arg;
      Config      := Config_Xml;
    end Start;
    
    Ada.Text_IO.Put_Line("Welcome to " & Banner & " console!");
    
    declare
    begin
      Parse_Configuration(Config);
    exception
      when Error : others => 
        Utility.Print_Exception(Error);
        Ada.Exceptions.Raise_Exception(Utility.Configuration_File_Error'Identity, "There is a problem with the configuration file.");
    end;
    
    Ada.Text_IO.Put(":> ");
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        declare
          User_Input : String := In_String(1 .. In_Last);
        begin
          Process_User_Input(User_Input, To_Controll);
          
          if Utility.Is_Equal(User_Input, "quit", true) OR 
             Utility.Is_Equal(User_Input, "exit", true) OR 
             Utility.Is_Equal(User_Input, "abort", true) 
          then
            exit;
          end if;
          
        exception
          when Error : others => Utility.Print_Exception(Error);
        end;
      end if;
        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Console;
  
  
end Generic_Console;