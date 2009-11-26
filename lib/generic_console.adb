with Ada.Text_IO;
with Ada.Exceptions;
with Utility;
with Xml_Parser;
package body Generic_Console is
  
  task body Console is
    In_String           : String(1..20);
    In_Last             : Natural;
    To_Controll         : To_Controll_Task_Access;
    Path_To_Config_File : ASU.Unbounded_String;
    
    procedure Read_and_Parse_Config_File(Config_File : String) is
    begin
      if Utility.Does_File_Exist(Config_File) then
        Ada.Text_IO.Put_Line("--> Parsing config file");
        Parse_Configuration(
          Xml_Parser.Parse(File_Name => Config_File)
        );
        Ada.Text_IO.Put_Line("--> Done");
      else
        Ada.Text_IO.Put_Line("No config file found!");
      end if;
    exception
      when Error : others => 
        Utility.Print_Exception(Error);
        Ada.Exceptions.Raise_Exception(Utility.Configuration_File_Error'Identity, "There is a problem with the configuration file.");
    end Read_and_Parse_Config_File;
    
    
  begin
    accept Start(M_Arg : To_Controll_Task_Access; Config_File : String) do
      To_Controll         := M_Arg;
      Path_To_Config_File := ASU.To_Unbounded_String(Config_File);
    end Start;
    
    Ada.Text_IO.Put_Line("Welcome to " & Banner & " console!");
    
    Read_and_Parse_Config_File(ASU.To_String(Path_To_Config_File));
    
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
          
          elsif Utility.Is_Equal(User_Input, "reload-config", true) then
            Read_and_Parse_Config_File(ASU.To_String(Path_To_Config_File));
          end if;
          
        exception
          when Error : others => Utility.Print_Exception(Error);
        end;
      end if;
        
      Ada.Text_IO.Put(":> ");

    end loop;
    
  end Console;
  
  
end Generic_Console;