
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Application_Helper;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;

with Logger;

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
        accept Start(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type) do
          Addr.Addr := Host;
          Addr.Port := Port;
        end Start;
        
        Logger.Put_Line("Server task started", Logger.Info);
        
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
        
        Logger.Put_Line("Ready to accept connections on " & Image(Addr.Addr) & " with port " & Addr.Port'Img & ".", Logger.Info);
        
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
        Logger.Put_Line("Terminating server task", Logger.Info);
        --  tidy up
        Close_Selector(Accept_Selector);
        Empty(Accept_Set);
        Close_Socket(Server);
        Finalize;
        exit;
      end select;
    end loop;
    Logger.Put_Line("Server task terminated", Logger.Info);
  exception
    when Error : others => 
      Close_Selector(Accept_Selector);
      Empty(Accept_Set);
      Close_Socket(Server);
      Finalize;
      Application_Helper.Print_Exception(Error);
  end Server_Task;

end Generic_Server;