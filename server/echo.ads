with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Strings.Unbounded;

with Master_Helper;

generic
  type My_Job is private;
  type Job_Entry_Record_Access is private;
  with procedure Add_Worker(New_Worker : Master_Helper.Worker_Record_Access);
  with function Get_Job_By_Id(Id : Natural) return Job_Entry_Record_Access;
  with function Get_Next_Pending_Job return Job_Entry_Record_Access;
  with procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State);
  with function Job_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;

package Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Echo; 
  type Echo_Access is access Echo;
  --
  task type Echo is
    entry Start (N_Sock : IN Socket_Type; Self : IN Echo_Access);
    entry ReStart (N_Sock : IN Socket_Type);
  end Echo;
  
end Echo;