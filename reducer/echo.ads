with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Strings.Unbounded;

generic
  with function Merge_Jobs return Boolean;
  with function Finalize return Boolean;

package Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Echo; 
  type Echo_Access is access Echo;
  --
  task type Echo is
    entry Start (N_Sock : IN Socket_Type; Self : IN Echo_Access);
  end Echo;
  
  
end Echo;