with GNAT.Sockets;
use GNAT.Sockets;
--with Echo;

--generic
--  type My_Job is private;
--  type Job_Entry_Record_Access is private;
--  with procedure Add_Worker(New_Worker : Master_Helper.Worker_Record_Access);
--  with function Get_Job_By_Id(Id : Natural) return Job_Entry_Record_Access;
--  with function Get_Next_Pending_Job return Job_Entry_Record_Access;
--  with procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State);
--  with function Job_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
    
package Mapper_Server is
  
--  package Echo_MR renames Echo;
  
--  package Echo_MR is new Echo
--  (
--    My_Job,
--    Job_Entry_Record_Access,
--    Add_Worker,
--    Get_Job_By_Id,
--    Get_Next_Pending_Job,
--    Change_Job_State,
--    Job_To_Xml
--  );
  
  task type Server_Task is
    entry Start;
    entry Stop;
  end Server_Task;
  
end Mapper_Server;