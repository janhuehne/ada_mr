with GNAT.Sockets;
use GNAT.Sockets;

package Echo is
  
  type Echo; 
  type Echo_Access is access Echo;
  --
  task type Echo is
    entry Start (N_Sock : IN Socket_Type; Self : IN Echo_Access);
    entry ReStart (N_Sock : IN Socket_Type);
  end Echo;
  
  
end Echo;