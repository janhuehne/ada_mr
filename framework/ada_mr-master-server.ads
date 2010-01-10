with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Master.Helper;

with Ada.Strings.Unbounded;

with Ada_Mr.Generics.Server;
with Ada_Mr.Generics.Echo;

with Ada_Mr.Xml;

with Ada_Mr.Helper;

generic
  type My_Job is private;
  type Job_Entry_Record_Access is private;
  with procedure Add_Worker(New_Worker : Ada_Mr.Master.Helper.Worker_Record_Access);
  with function Find_Worker_By_Identifier(Identifier : String) return Ada_Mr.Master.Helper.Worker_Record_Access;
  with function Find_Worker_By_Access_Token_And_Type(Access_Token : String; W_Type : Ada_Mr.Helper.Worker_Type) return Ada_Mr.Master.Helper.Worker_Record_Access;
  with function Get_Job_By_Id(Id : Natural) return Job_Entry_Record_Access;
  with function Get_Next_Pending_Job return Job_Entry_Record_Access;
  with procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Ada_Mr.Master.Helper.Job_State; Message : String := "");
  with function Job_Entry_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
  with procedure Stop_Master;
    
package Ada_Mr.Master.Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server return Boolean;
  procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access);
  
  package Server is new Ada_Mr.Generics.Server(
    Exit_Server,
    Process_Request,
    Stop_Master
  );
  
end Ada_Mr.Master.Server;