
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Utility;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;

with Master_Helper;

package body Generic_Server is
  use GNAT.Sockets;
  
  task body Server_Task is
    Server          : Socket_Type;
    New_Sock        : Socket_Type;
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
        accept Start(Host : String; Port : GNAT.Sockets.Port_Type) do
          Addr.Addr := Inet_Addr(Host);
          Addr.Port := Port;
        end Start;
        
        Initialize;
        Create_Socket(Server);
        
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
        Ada.Text_IO.Put_Line("-> Ready to accept connections on port " & Addr.Port'Img & ".");
        
        loop
          exit when Exit_Server;
          
          --  check for input (connection requests) on Server socket 
          Set(Accept_Set, Server);
          
          --  time-out on check if no request within 1 second 
          Check_Selector(Accept_Selector, Accept_Set, WSet, Accept_Status, 1.0);
          
          if Accept_Status = Completed then 
            --  must be an event on Server socket as it is the only 
            --  one that we are checking. 
            --  Hence the Accept_Socket call should not block. 
            Accept_Socket(Server, New_Sock, Peer_Addr);
            
            Process_Incomming_Connection(New_Sock);
          end if;
        end loop;
        
      or
        accept Stop;
        
        --  tidy up
        Close_Selector(Accept_Selector);
        Empty(Accept_Set);
        Close_Socket(Server);
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("-> Server task stopped.");
        Finalize;
        exit;
      end select;
    end loop;
  end Server_Task;

end Generic_Server;