with Ada.Text_IO;
with Utility;
with Xml;
with Xml_Parser;
with Xml_Helper;

package body Echo is
  
    task body Echo is 
      Sock            : Socket_Type;
      S               : Stream_Access;
      Me              : Echo_Access;
      Input_Selector  : Selector_Type;
      Input_Set       : Socket_Set_Type;
      WSet            : Socket_Set_Type;
      Input_Status    : Selector_Status;
      
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
          
          loop
            -- check for input on Sock socket 
            Set(Input_Set, Sock);
            
            -- time-out on check if no input within 0.5 second 
            Check_Selector(Input_Selector, Input_Set, WSet, Input_Status, 0.5);
            
            if Input_Status = Completed then
              -- we have input, so process it 
              declare
                Str : String := String'Input(S);
              begin
                Ada.Text_IO.Put_Line(Str);
              end;
              
              String'Output(
                S, 
                Xml_Helper.Create_System_Control(Xml_Helper.Reducer, "okay")
              );
              
            end if;
            
--            if Server.Aborted.Check = true then
--              exit;
--            end if;
          
          end loop;
          
          Ada.Text_IO.New_Line;
          Ada.Text_IO.Put_Line("Slave Closing Connection");
          ShutDown_Socket(Sock, Shut_Read_Write);
        end;
      
      end loop;
    end Echo;
  
end Echo;