with Xml;
with Ada.Strings.Unbounded;
generic
  type To_Controll_Task_Access is private;
  with function Banner return String;
  with procedure Parse_Configuration(Config_Xml : Xml.Node_Access);
  with procedure Process_User_Input(User_Input : String; To_Controll : To_Controll_Task_Access);

package Generic_Console is
  
  package ASU renames Ada.Strings.Unbounded;
  
  task type Console is
    entry Start(M_Arg : To_Controll_Task_Access);
  end Console;
  
end Generic_Console;