with Ada.Strings.Unbounded;

generic
  type To_Controll_Task_Access is private;
  with function Banner return String;
  with procedure Process_User_Input(User_Input : String; To_Controll : To_Controll_Task_Access);
    
package Ada_Mr.Generics.Console is
  
  package ASU renames Ada.Strings.Unbounded;
  
  task type Console is
    entry Start(M_Arg : To_Controll_Task_Access);
  end Console;
  
end Ada_Mr.Generics.Console;