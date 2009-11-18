with Ada.Containers.Vectors;
with Runner;
with Xml;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String);
  with procedure Compute_Job(Job : in My_Job);
  with function Job_Result_To_Xml return String;
  
package Mapper is
  
  package Runner_MR is new Runner(My_Job, From_Xml, To_Xml, Compute_Job, Job_Result_To_Xml);
  
  type Mapper_Task;
  type Mapper_Task_Access is access Mapper_Task;
  
  task type Mapper_Task is
    entry Start;
    entry Stop;
  end Mapper_Task;
  
  task type Console is
    entry Start(C_Arg : Mapper_Task_Access);
  end Console;
  
end Mapper;