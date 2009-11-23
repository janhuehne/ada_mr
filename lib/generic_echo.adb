with Ada.Text_IO;
with Utility;
with Xml;
with Xml_Parser;

with Xml_Helper;
with Ada.Exceptions;


package body Generic_Echo is

  task body Echo is 
    Sock : Socket_Type;
    S : Stream_Access;
    Input_Selector : Selector_Type;
    Input_Set : Socket_Set_Type;
    WSet : Socket_Set_Type;
    Input_Status : Selector_Status;
  begin 
    --set up selector 
    Create_Selector(Input_Selector);
    
    -- Initialise socket sets 
    -- WSet is always empty as we are not interested in output events 
    -- RSet only ever contains one socket namely Sock 
    Empty(Input_Set);
    Empty(WSet);
    
    ACCEPT Start(N_Sock : IN Socket_Type) DO
      Sock := N_Sock;
    end Start;
    
    loop
      -- block for exception handling
      begin
        
        -- set up stream on socket
        S := Stream(Sock);
        
        -- acknowledge connection
        Boolean'Write(S, True);
        
        loop
          -- check for input on Sock socket 
          Set(Input_Set, Sock);
          
          -- time-out on check if no input within 0.5 second 
          Check_Selector(Input_Selector, Input_Set, WSet, Input_Status, 0.5);
          
          -- we have input, so process it 
          if Input_Status = Completed then
            Process_Request(S);
            exit; -- Shutdown socket after request is processed
          end if;
          
        end loop;
        
        ShutDown_Socket(Sock, Shut_Read_Write);
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
  
end Generic_Echo;