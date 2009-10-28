with Ada.Containers.Vectors;

generic
  type My_Job is private;
  with function From_Xml(Xml : in String) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String);
  
package Client is
  
  type Client_Task;
  type Client_Task_Access is access Client_Task;
  
  task type Client_Task is
--    entry Import_Jobs(Jobs : Job_Vector.Vector);
--    entry Start_Master(M : Master_Task_Access);
--    entry Split_Data;
--    entry Add_Job;
--    entry Stop_Master;
    entry Say_Hello;
  end Client_Task;
  
  task type Console is
    entry Start(C_Arg : Client_Task_Access);
  end Console;
  
end Client;