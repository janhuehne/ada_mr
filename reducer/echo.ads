with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Strings.Unbounded;
with Xml;

generic
  with function Merge_Jobs(Xml_Node : Xml.Node_Access) return Boolean;
  with function Finalize_Jobs return Boolean;

package Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Echo; 
  type Echo_Access is access Echo;
  --
  task type Echo is
    entry Start (N_Sock : IN Socket_Type; Self : IN Echo_Access);
  end Echo;
  
  
end Echo;