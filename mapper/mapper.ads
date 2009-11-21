with Ada.Containers.Vectors;
with Mapper_Runner;
with Xml;
with GNAT.Sockets;
with Mapper_Server;
with Ada.Strings.Unbounded;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String);
  with procedure Compute_Job(Job : in My_Job);
  with function Job_Result_To_Xml return String;
  
package Mapper is
  
  package ASU renames Ada.Strings.Unbounded;
    
  package Runner is new Mapper_Runner(My_Job, From_Xml, To_Xml, Compute_Job, Job_Result_To_Xml);
  
  type Mapper_Task;
  type Mapper_Task_Access is access Mapper_Task;
  
  task type Mapper_Task is
    entry Start;
    entry Stop;
  end Mapper_Task;
  
  task type Console is
    entry Start(C_Arg : Mapper_Task_Access; Config_Xml : Xml.Node_Access);
  end Console;
  
  
  package Server renames Mapper_Server;
  --  package Mapper_Server is new Server(
  --    My_Job,
  --    Job_Entry_Record_Access,
  --    Worker.Add,
  --    Jobs.Get_By_Id,
  --    Jobs.Get_Next_Pending,
  --    Change_Job_State,
  --    Job_To_Xml
  --  );
  
  
  
  -- global configs
  Listen_On_Port : GNAT.Sockets.Port_Type := 7100;
  
  
  
  
end Mapper;