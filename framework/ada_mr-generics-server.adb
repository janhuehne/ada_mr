
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada_Mr.Helper;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;

with Ada_Mr.Logger;

package body Ada_Mr.Generics.Server is
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
    
    Total_Tasks     : Natural := 0;
    Slave           : Echo.Echo_Access;
  begin
    loop
      select
        accept Start(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type) do
          Addr.Addr := Host;
          Addr.Port := Port;
        end Start;
        
        Ada_Mr.Logger.Put_Line("Server task started", Ada_Mr.Logger.Info);
        
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
        
        Ada_Mr.Logger.Put_Line("Ready to accept connections on " & Image(Addr.Addr) & " with port " & Addr.Port'Img & ".", Ada_Mr.Logger.Info);
        
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
            
            
            if Echo.Buffer.Num_Waiting = 0 and Total_Tasks < Echo.Max_Tasks then
              -- start new task
              Slave := new Echo.Echo;
              Total_Tasks := Total_Tasks + 1;
              Logger.Put_Line("New echo task started", Logger.Info);

              -- call entry Start to activate task
              Slave.Start(New_Sock, Slave);
            else
              Logger.Put_Line("Waiting for an idle echo task", Logger.Info);
              Echo.Buffer.Extract(Slave);
              
              -- call entry Start to re-activate task
              Slave.ReStart(New_Sock);
              Logger.Put_Line("Idle echo task reactivated", Logger.Info);
            end if;

            --Process_Incomming_Connection(New_Sock);
          end if;
        end loop;
          
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line("Terminating server task", Ada_Mr.Logger.Info);
        --  tidy up
        declare
        begin
          Close_Selector(Accept_Selector);
          Empty(Accept_Set);
          Close_Socket(Server);
          Finalize;
        exception
          when others => null;
        end;
        exit;
      end select;
    end loop;
    Ada_Mr.Logger.Put_Line("Server task terminated", Ada_Mr.Logger.Info);
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      
      Close_Selector(Accept_Selector);
      Empty(Accept_Set);
      Close_Socket(Server);
      Finalize;
  end Server_Task;

end Ada_Mr.Generics.Server;