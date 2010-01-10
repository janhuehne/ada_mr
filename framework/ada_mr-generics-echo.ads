with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Xml;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;

generic
  with procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access);
    
package Ada_Mr.Generics.Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Echo; 
  type Echo_Access is access Echo;
  
  task type Echo is
    entry Start (N_Sock : IN Socket_Type);
    entry ReStart (N_Sock : IN Socket_Type);
  end Echo;
  
end Ada_Mr.Generics.Echo;