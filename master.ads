with Ada.Containers.Vectors;
with Echo;

generic
  type My_Job is private;
  with function From_Xml(Xml : in String) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_id(Job : in My_Job) return Natural;
  
package Master is
  
  
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
    
  type Master_Task;
  type Master_Task_Access is access Master_Task;
  
  task type Master_Task is
    entry Import_Jobs(Jobs : Job_Vector.Vector);
    entry Start_Master(M : Master_Task_Access);
    entry Split_Data;
    entry Add_Job;
    entry Stop_Master;
    entry Say_Hello;
  end Master_Task;
  
--  type Job_Management_Task;
--  type Job_Management_Task_Access is access Job_Management_Task;
--  
--  task type Job_Management_Task;
  
  
  task type Master_Console is
    entry Start(M_Arg : Master_Task_Access);
  end Master_Console;
  
  Unprocessed_Jobs : Job_Vector.Vector;
--  Jobs_In_Progress : Job_Vector.Vector;
--  Processed_Jobs   : Job_Vector.Vector;
  
  procedure Add_New_Job(Job : My_Job);
  function Get_Next_Job(Remove_From_Vector : Boolean := true) return My_Job;
end Master;