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

with Master_Helper;

package body Server is 
  
  task body Server_Task is
    Server          : Socket_Type;
    New_Sock        : Socket_Type;
    Slave           : Echo_MR.Echo_Access;
    Addr            : Sock_Addr_Type;
    Peer_Addr       : Sock_Addr_Type;
    Avail           : Boolean := False;
    TotalTasks      : Natural := 0;
    Accept_Selector : Selector_Type;
    Accept_Set      : Socket_Set_Type;
    WSet            : Socket_Set_Type;
    Accept_Status   : Selector_Status;
  begin
    loop
      select
        accept Start;
        Initialize;
        Create_Socket(Server);
        
--        Addr.Addr := Addresses(Get_Host_By_Name ("127.0.0.1"), 1);
        Addr.Addr := Addresses(Get_Host_By_Name ("192.168.178.108"), 1);

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
        
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("-> Ada MR Master is ready to accept connections on port " & Addr.Port'Img & ".");
        
        loop
          exit when Master_Helper.Aborted.Check_Master = true;
          
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
            
            Slave := new Echo_MR.Echo;
            
            Ada.Text_IO.Put_Line ( "-> New incomming task" );
            
            Slave.Start(New_Sock, Slave);
          end if;
        end loop;
        
      or
        accept Stop;
        Master_Helper.Aborted.Stop_Clients;
        
        --  tidy up 
        Close_Selector(Accept_Selector);
        Empty(Accept_Set);
        Close_Socket(Server);
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("-> Ada MR Master server task stopped.");
        Finalize;
        exit;
      end select;
    end loop;
  end Server_Task;
    
end Server;