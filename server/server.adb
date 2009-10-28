-------------------------------------------------------------- 
-- 
--  Pool_Server 
-- 
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Utility;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;

package body Server is 
  
  protected body Aborted is
  
    procedure Stop_Master is
    begin
      Abort_Master := true;
    end Stop_Master;
    
    
    procedure Stop_Clients is
    begin
      Abort_Clients := true;
    end Stop_Clients;
    
    function Check_Master return Boolean is
    begin
      return Abort_Master;
    end Check_Master;
    
    function Check_Clients return Boolean is
    begin
      return Abort_Clients;
    end Check_Clients;
    
  end Aborted;
  
  

--  
--  protected body Buffer is
--    entry Deposit(X : IN Echo_Access) when Count < MaxTasks is
--    begin 
--      Buf (I) := X;
--      I := I + 1;
--      Count := Count + 1;
--    end Deposit;
--    
--    entry Extract(X : OUT Echo_Access) when Count > 0 is
--    begin 
--      X := Buf (J);
--      J := J + 1;
--      Count := Count - 1;
--    end Extract;
--    
--    function NumWaiting return Natural is
--    begin
--      return Count;
--    end NumWaiting;
--  end Buffer;
--  
  
  task body P_Server is
  begin
    loop
      select
        accept Start;
        Initialize;
        Create_Socket(Server);
        
        Addr.Addr := Addresses(Get_Host_By_Name ("127.0.0.1"), 1);
        Addr.Port := 7000;
        
        --  allow server address to be reused for multiple connections 
        Set_Socket_Option(Server, Socket_Level, (Reuse_Address, True));
        Bind_Socket(Server, Addr);
        Listen_Socket(Server, 4);
        
        --  set up selector 
        Create_Selector(Accept_Selector);
        
        --  Initialise socket sets 
        --  WSet is always empty as we are not interested in output 
        --  events Accept_Set only ever contains one socket namely 
        --  Server 
        Empty(Accept_Set);
        Empty(WSet);
        
        loop 
          exit when Aborted.Check_Master = true;
          
          --  check for input (connection requests) on Server socket 
          Set(Accept_Set, Server);
          
          --  time-out on check if no request within 1 second 
          Check_Selector(Accept_Selector, Accept_Set, WSet, Accept_Status, 1.0);
          
          if Accept_Status = Completed then 
            --  must be an event on Server socket as it is the only 
            --  one that we are checking. 
            --  Hence the Accept_Socket call should not block. 
            Accept_Socket(Server, New_Sock, Peer_Addr);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line("Connection accepted -- allocating slave");
            
    ----        if Buffer.NumWaiting = 0  and TotalTasks < MaxTasks then
    --          -- start new task 
            Slave := new Echo.Echo;
    --          TotalTasks := TotalTasks + 1;
            Ada.Text_IO.Put_Line ( "New slave task started" );
    --          
    --          --  call entry Start to activate task 
            Slave.Start(New_Sock, Slave);
    ----        else 
    ----          Ada.Text_IO.Put_Line("Waiting for an idle slave task");
    ----          Buffer.Extract (Slave);
    ----          --  call entry Start to re-activate task 
    ----          Slave.ReStart (New_Sock);
    ----          Ada.Text_IO.Put_Line ("Idle slave task reactivated");
    ----        end if;
          end if;
        end loop;
          
      or
        accept Stop;
        Aborted.Stop_Clients;
        --  tidy up 
        Close_Selector(Accept_Selector);
        Empty(Accept_Set);
        Close_Socket(Server);
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("Main server task exiting ...");
        Finalize;
        exit;
      end select;
    end loop;
  end P_Server;
    
end Server;