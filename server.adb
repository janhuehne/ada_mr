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

with Master;

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
  
  
  task body Echo is 
    Sock : Socket_Type;
    S : Stream_Access;
    Me : Echo_Access;
    Input_Selector : Selector_Type;
    Input_Set : Socket_Set_Type;
    WSet : Socket_Set_Type;
    Input_Status : Selector_Status;
  begin 
    --set up selector 
    Create_Selector(Input_Selector);
    
    --Initialise socket sets 
    --WSet is always empty as we are not interested in output events 
    -- RSet only ever contains one socket namely Sock 
    Empty(Input_Set);
    Empty(WSet);
    
    ACCEPT Start(N_Sock : IN Socket_Type; Self : IN Echo_Access) DO
      Sock := N_Sock;
      Me := Self;
    end Start;
    
    loop
      -- block for exception handling
      begin
        -- set up stream on socket
        S := Stream(Sock);
        
        -- acknowledge connection
        Boolean'Write(S, True);
        
        
        --String'Output(S, "<xml version=""1.0"" />Initialier XML Kram ...");
        
        loop
          -- check for input on Sock socket 
          Set(Input_Set, Sock);
          
          -- time-out on check if no input within 0.5 second 
          Check_Selector(Input_Selector, Input_Set, WSet, Input_Status, 0.5);
          
          Ada.Text_IO.Put(".");
          if Input_Status = Completed then
            -- we have input, so process it 

            declare
              Str : String := String'Input(S);
            begin
--              Ada.Text_IO.Put_Line(String'Input(S));
--              exit when Str = "quit";
--              
              if Utility.Starts_With(Str, "<?xml") then
                Ada.Text_IO.Put_Line("XML found!");
                String'Output(S, "XML received: " & Str);
                
                declare
                  Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
                  Client_Type : String := Xml.Get_Value(Xml_Root, "client-type");
                begin
                  Master.Master_Task.Say_Hello;
                  null;
--                  if Utility.Is_Equal(Client_Type, "Mapper") or Utility.Is_Equal(Client_Type, "Reducer") then
--                    Ada.Text_IO.Put_Line("Was gefunden!");
--                  else
--                    Ada.Text_IO.Put_Line("Mist. Was anderes!");
--                  end if;
                end;
                
              else
                Ada.Text_IO.Put_Line("Unknown command");
                String'Output(S, "Unknown command: " & Str);
              end if;
--              
            end;
          end if;
          
          if Aborted.Check_Clients then
            String'Output(S, "Server aborted");
            exit;
          end if;
            
        end loop;
          
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("Slave Closing Connection");
        ShutDown_Socket(Sock, Shut_Read_Write);
--        Buffer.Deposit(Me);
        
--        exception 
          -- The mostly likely exception is if client quits unexpectedly 
          -- close the socket and deposit ourselves in the buffer 
--          when others => 
--               Ada.Text_IO.New_Line;
--               Ada.Text_IO.Put_Line("Connection closed unexpectedly");
--               Close_Socket(Sock);
--               Buffer.Deposit(Me);
      end;
      
      select
        ACCEPT ReStart (N_Sock : IN Socket_Type) DO
          Sock := N_Sock;
        end ReStart;
      or
        -- terminate if all slaves are queued here and 
        -- if the main server task has finished 
        terminate;
      end select;
    end loop;
  end Echo;
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
    Master_Task : Master_Task_Access;
  begin
    loop
      select
        accept Start(Master_Task : Master_Task_Access) do
          Master_Task := Master_Task;
        end Start;
        Ada.Text_IO.Put_Line("AABBCC");
        
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
            Slave := NEW Echo;
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