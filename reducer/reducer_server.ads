with GNAT.Sockets;
use GNAT.Sockets;
with Reducer_Helper;

with Ada.Strings.Unbounded;

with Generic_Server;
with Generic_Echo;

with Xml;
with Utility;

generic
  with procedure Finalize_Jobs;
  with procedure Stop_Reducer;

package Reducer_Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server return Boolean;
  procedure Process_Incomming_Connection(New_Sock : Socket_Type);
  procedure Process_Request(S : Stream_Access; From : Utility.Worker_Type; Xml_Root : Xml.Node_Access);
  
  package Server is new Generic_Server(
    Exit_Server,
    Process_Incomming_Connection,
    Stop_Reducer
  );
  
  package Echo_MR is new Generic_Echo(
    Process_Request
  );
  
end Reducer_Server;