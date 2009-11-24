with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Strings.Unbounded;

with Generic_Server;

package Mapper_Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server return Boolean;
  procedure Process_Incomming_Connection(New_Sock : Socket_Type);
  procedure Process_Request(S : Stream_Access);
  
  package Server is new Generic_Server(
    Exit_Server,
    Process_Incomming_Connection
  );
  
--  package Echo_MR is new Generic_Echo(
--    Process_Request
--  );
  
end Mapper_Server;