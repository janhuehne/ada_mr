with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Strings.Unbounded;

with Generic_Server;
with Application_Helper;
with Xml;

generic
  with procedure Stop_Mapper;

package Mapper_Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server return Boolean;
  procedure Process_Incomming_Connection(New_Sock : Socket_Type);
  procedure Process_Request(S : Stream_Access; From : Application_Helper.Worker_Type; Xml_Root : Xml.Node_Access);
  
  package Server is new Generic_Server(
    Exit_Server,
    Process_Incomming_Connection,
    Stop_Mapper
  );
  
--  package Echo_MR is new Generic_Echo(
--    Process_Request
--  );
  
end Mapper_Server;