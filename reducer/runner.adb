with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Utility;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;
with Xml_Helper;

with Reducer_Helper;

with Ada.Exceptions;

package body Runner is 
  
  task body Runner_Task is
    Server          : Socket_Type;
    New_Sock        : Socket_Type;
    Slave           : Echo_MR.Echo_Access;
    Addr            : Sock_Addr_Type;
    Peer_Addr       : Sock_Addr_Type;
    Avail           : Boolean := False;
    Accept_Selector : Selector_Type;
    Accept_Set      : Socket_Set_Type;
    WSet            : Socket_Set_Type;
    Accept_Status   : Selector_Status;
  begin
    loop
      select
        accept Start;
          Ada.Text_IO.Put_Line("Reducer Task started!");
          
          Initialize;
          Create_Socket(Server);
          
          Addr.Addr := Addresses(Get_Host_By_Name ("127.0.0.1"), 1);
          Addr.Port := 7100;
          
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
            exit when Reducer_Helper.Aborted.Check = true;
            
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
              
              Slave := new Echo_MR.Echo;
              Ada.Text_IO.Put_Line ( "New slave task started" );
              
              --  call entry Start to activate task 
              Slave.Start(New_Sock, Slave);
            end if;

          end loop;
      or
        accept Stop;
          Close_Selector(Accept_Selector);
          Empty(Accept_Set);
          Close_Socket(Server);
          Ada.Text_IO.New_Line;
          Ada.Text_IO.Put_Line("Reducer exiting ...");
          Finalize;
          exit;
          
      end select;
    end loop;
  exception 
    when Error : others =>
      Ada.Text_IO.Put_Line ("Exception: Reducer quitting ..." ) ;
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
      
      Close_Socket(Server);
      Close_Selector(Accept_Selector);
      Finalize;
  end Runner_Task;
  
end Runner;