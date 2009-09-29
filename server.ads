with GNAT.Sockets;
use GNAT.Sockets;

package Server is
  
  protected Aborted is
    procedure Stop_Master;
    procedure Stop_Clients;
    function Check_Master return Boolean;
    function Check_Clients return Boolean;
  private
    Abort_Master  : Boolean := false;
    Abort_Clients : Boolean := false;
  end Aborted;
  
  
  type Echo; 
  type Echo_Access is access Echo;
  --
  task type Echo is
    entry Start (N_Sock : IN Socket_Type; Self : IN Echo_Access);
    entry ReStart (N_Sock : IN Socket_Type);
  end Echo;
  
  
--  type Task_Array is array ( Index ) of Echo_Access;
--  
--  
--  protected Buffer is
--    entry Deposit (X : in Echo_Access);
--    entry Extract (X : out Echo_Access);
--    
--    function NumWaiting return Natural;
--    
--    private
--      Buf : Task_Array;
--      I, J : Index := 0;
--      Count : Natural range 0 .. MaxTasks := 0;
--  end Buffer;
  
  Server          : Socket_Type;
  New_Sock        : Socket_Type;
  Slave           : Echo_Access;
  Addr            : Sock_Addr_Type;
  Peer_Addr       : Sock_Addr_Type;
  Avail           : Boolean := False;
  Ch              : Character;
  TotalTasks      : Natural := 0;
  Accept_Selector : Selector_Type;
  Accept_Set      : Socket_Set_Type;
  WSet            : Socket_Set_Type;
  Accept_Status   : Selector_Status;
  
  task type P_Server is
    entry Start(Master_Task : Master.Master_Task_Access);
    entry Stop;
  end P_Server;
  
  
  
  
end Server;