with Ada.Text_IO;
with Utility;
with Xml;
with Xml_Parser;
with Xml_Helper;
with Reducer_Helper;
with Ada.Exceptions;

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
                if Xml_Helper.Is_Valid_Xml_String(Str) then
                  
                  declare
                    Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
                  begin
                    if Utility.Is_Equal(Xml.Get_Tag(Xml_Root), "adamr-mapper") then
                      
                      if Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "job_result") then
                        Reducer_Helper.Finished_Jobs_Queue.Append(Xml.Find_Child_With_Tag(Xml_Root, "details"));
                        
--                        String'Output(
--                          S, 
--                          Xml_Helper.Create_System_Control(Xml_Helper.Reducer, "okay")
--                        );
                      end if;
                    elsif Utility.Is_Equal(Xml.Get_Tag(Xml_Root), "adamr-master") then
                      if Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "finalize") then
                        if Finalize_Jobs then
                          Ada.Text_IO.Put_Line("Job done!");
                        else
                          Ada.Text_IO.Put_Line("Error in finalize call");
                        end if;
                      end if;
                    end if;
                  exception
                    when Error : others =>
                      Ada.Text_IO.Put_Line ("Error!!" ) ;
                      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(Error));
                      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(Error));
                      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
                  end;
                  
                else
                  Ada.Text_IO.Put_Line("No valid xml string!");
                  Ada.Text_IO.Put_Line(Str);
                end if;
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