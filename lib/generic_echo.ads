with GNAT.Sockets;
use GNAT.Sockets;
with Xml;
with Ada.Strings.Unbounded;
with Utility;

generic
  with procedure Process_Request(S : Stream_Access; From : Utility.Worker_Type; Xml_Root : Xml.Node_Access);
    
package Generic_Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Echo; 
  type Echo_Access is access Echo;
  
  task type Echo is
    entry Start (N_Sock : IN Socket_Type);
    entry ReStart (N_Sock : IN Socket_Type);
  end Echo;
  
end Generic_Echo;