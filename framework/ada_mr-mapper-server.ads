with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Strings.Unbounded;

with Ada_Mr.Generics.Server;
with Ada_Mr.Helper;
with Ada_Mr.Xml;

generic
  with procedure Stop_Mapper;

package Ada_Mr.Mapper.Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server return Boolean;
  procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access);
  
  package Server is new Ada_Mr.Generics.Server(
    Exit_Server,
    Process_Request
--    Stop_Mapper
  );
  
end Ada_Mr.Mapper.Server;