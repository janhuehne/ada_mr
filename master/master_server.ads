with GNAT.Sockets;
use GNAT.Sockets;
with Master_Helper;

with Ada.Strings.Unbounded;

with Generic_Server;
with Generic_Echo;

with Xml;

with Utility;

generic
  type My_Job is private;
  type Job_Entry_Record_Access is private;
  with procedure Add_Worker(New_Worker : Master_Helper.Worker_Record_Access);
  with function Find_Worker_By_Identifier(Identifier : String) return Master_Helper.Worker_Record_Access;
  with function Find_Worker_By_Access_Token_And_Type(Access_Token : String; W_Type : Utility.Worker_Type) return Master_Helper.Worker_Record_Access;
  with function Get_Job_By_Id(Id : Natural) return Job_Entry_Record_Access;
  with function Get_Next_Pending_Job return Job_Entry_Record_Access;
  with procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State; Message : String := "");
  with function Job_Entry_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
  with procedure Stop_Master;
    
package Master_Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server return Boolean;
  procedure Process_Incomming_Connection(New_Sock : Socket_Type);
  procedure Process_Request(S : Stream_Access; From : Utility.Worker_Type; Xml_Root : Xml.Node_Access);
  
  package Server is new Generic_Server(
    Exit_Server,
    Process_Incomming_Connection,
    Stop_Master
  );
  
  package Echo_MR is new Generic_Echo(
    Process_Request
  );
  
  
  
  
  
end Master_Server;