with Ada.Text_IO;
with Utility;
with Xml;
with Xml_Parser;
with Server;
with Worker;
with Xml_Queue;
with Xml_Helper;

package body Echo is
  
    task body Echo is 
      Sock : Socket_Type;
      S : Stream_Access;
      Me : Echo_Access;
      Input_Selector : Selector_Type;
      Input_Set : Socket_Set_Type;
      WSet : Socket_Set_Type;
      Input_Status : Selector_Status;
      
      Initialization_Complete : Boolean := false;
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

--            Ada.Text_IO.Put(".");
            if Input_Status = Completed then
              -- we have input, so process it 
              
              declare
                Str : String := String'Input(S);
              begin
                Ada.Text_IO.Put_Line(Str);
                
                if Utility.Starts_With(Str, "<?xml") then
                  Ada.Text_IO.Put_Line("XML found!");
--                  String'Output(S, "XML received: " & Str);
                  
                  declare
                    Xml_Root    : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
                  begin
                    
                    if Utility.Is_Equal(Xml.Get_Tag(Xml_Root), "adamr-mapper") then
                      
                      if Initialization_Complete = false then
                        
                        if Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "initialization") then
                          Worker.Add_New_Worker(Xml.Find_Child_With_Tag(Xml_Root, "details"), Me);
                          Initialization_Complete := true;
                          String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, "okay"));
                        else
                          Ada.Text_IO.Put_Line("Initialization missing.");
                          String'Output(S, "Initialization missing.");
                        end if;
                        
                      else
                        
                        if Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "job_request") then
                          
                          if Xml_Queue.Count_Jobs(Xml_Queue.Pending) > 0 then
                          
                            declare
                              Job : Xml_Queue.Xml_Job_Entry_Access := Xml_Queue.Find_First_Job_By_State(Xml_Queue.Pending);
                            begin
                              Xml_Queue.Change_Job_State(Job, Xml_Queue.In_Progress);
                              String'Output(S, ASU.To_String(Job.Xml));
                            end;
                          else
                            String'Output(S, "<?xml version=""1.0"" ?><adamr-master><sysctrl><message>quit</sysctrl></adamr-master>");
                          end if;
                            
                        elsif Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "change_job_state") then
                          
                          Ada.Text_IO.Put_Line("Entering change_job_state");
                          
                          declare
                            Xml_Job_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "details");
                          begin
                            Xml_Queue.Change_Job_State(Xml.Get_Value(Xml_Job_Details, "id"), Xml.Get_Value(Xml_Job_Details, "state"));
                            String'Output(S, "<?xml version=""1.0"" ?><adamr-master><sysctrl><message>ok</sysctrl></adamr-master>");
                          exception
                            when Xml_Queue.Invalid_Job_State => 
                              Ada.Text_IO.Put_Line("Unknow job state");
                              String'Output(S, "Unknown job state.");
                            when others =>
                              Ada.Text_IO.Put_Line("Can not change job state");
                              String'Output(S, "Can not change job state");
                          end;
                          
                        else
                          Ada.Text_IO.Put_Line("Unknown command.");
                          String'Output(S, "Unknown command.");
                        end if;
                      
                      end if;
                    
                    end if;
                  
                  exception
                    when Xml.Node_Not_Found =>
                      Ada.Text_IO.Put_Line("Can't play with the xml request!");
                      
                    when Worker.Invalid_Worker =>
                      String'Output(S, "Unknow worker type tries to register.");
                      Ada.Text_IO.Put_Line("Unknow worker type tries to register.");
                      exit;
                  end;
                  
                else
                  Ada.Text_IO.Put_Line("Unknown command");
                  String'Output(S, "Unknown command: " & Str);
                end if;

              end;
            end if;

            if Server.Aborted.Check_Clients then
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
  
end Echo;