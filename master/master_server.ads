with GNAT.Sockets;
use GNAT.Sockets;
with Master_Helper;

with Ada.Strings.Unbounded;

with Generic_Server;
with Generic_Echo;

generic
  type My_Job is private;
  type Job_Entry_Record_Access is private;
  with procedure Add_Worker(New_Worker : Master_Helper.Worker_Record_Access);
  with function Get_Job_By_Id(Id : Natural) return Job_Entry_Record_Access;
  with function Get_Next_Pending_Job return Job_Entry_Record_Access;
  with procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State);
  with function Job_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
    
package Master_Server is
  
  package ASU renames Ada.Strings.Unbounded;
--  package Echo_MR is new Echo(
--    My_Job,
--    Job_Entry_Record_Access,
--    Add_Worker,
--    Get_Job_By_Id,
--    Get_Next_Pending_Job,
--    Change_Job_State,
--    Job_To_Xml
--  );
  
  function Exit_Server return Boolean;
  procedure Process_Incomming_Connection(New_Sock : Socket_Type);
  procedure Process_Request(S : Stream_Access);
  
  package Server is new Generic_Server(
    Exit_Server,
    Process_Incomming_Connection
  );
  
  package Echo_MR is new Generic_Echo(
    Process_Request
  );
  
  
  
  
  
--  task type Server_Task is
--    entry Start;
--    entry Stop;
--  end Server_Task;
  
end Master_Server;