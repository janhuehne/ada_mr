with Ada.Text_IO;
with Utility;
with Xml;
with Xml_Parser;
with Server;
--with Worker;
with Xml_Queue;
with Xml_Helper;
with Master_Helper;
with Ada.Exceptions;

package body Echo is
  
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
      
      -- Initialise socket sets 
      -- WSet is always empty as we are not interested in output events 
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
                Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
              begin
                Ada.Text_IO.Put_Line("XML Request: " & Str);
                  
                  if Xml_Helper.Is_Mapper_Request(Xml_Root) then
                    
                    if Xml_Helper.Is_Command(Xml_Root, "initialization") then
                      
                      declare
                        Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "details");
                        Worker_Entry : Master_Helper.Worker_Record_Access := new Master_Helper.Worker_Record;
                      begin
                        Worker_Entry.Identifier := ASU.To_Unbounded_String(Xml.Get_Value(Details, "identifier"));
                        Worker_Entry.W_Type     := Master_Helper.String_To_Worker_Type(Xml.Get_Value(Details, "type"));
                        
                        Add_Worker(Worker_Entry);
                        
                        String'Output(S, Xml_Helper.Xml_Command(
                          Xml_Helper.Master,
                          "new_access_token",
                          "<access_token>" & Worker_Entry.Access_Token & "</access_token>"
                        ));
                      end;
                      
                    elsif Xml_Helper.Is_Command(Xml_Root, "job_request") then
                      declare
                        Job_Entry : Job_Entry_Record_Access;
                      begin
                        Job_Entry := Get_Next_Pending_Job;
                        
                        String'Output(S, Xml_Helper.Xml_Command(Xml_Helper.Master, "new_job", Job_To_Xml(Job_Entry)));
                      exception
                        when Master_Helper.No_Job_Found =>
                          String'Output(S, Xml_Helper.Xml_Command(Xml_Helper.Master, "sleep", "<seconds>10</seconds>"));
                        when Error : others => 
                          Utility.Print_Exception(Error);
                          --Change_Job_State(Job_Entry, Master_Helper.Pending);
                      end;
                      
                    elsif Xml_Helper.Is_Command(Xml_Root, "job_done") then
                      declare
                        Details   : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "details");
                        Job_Entry : Job_Entry_Record_Access := Get_Job_By_Id(Integer'Value(Xml.Get_Value(Details, "job_id")));
                      begin
                        Change_Job_State(Job_Entry, Master_Helper.Done);
                        String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, "okay"));
                      end;
                    else
                      Ada.Exceptions.Raise_Exception(Master_Helper.Unknown_Command'Identity, "The command """ & Xml.Get_Value(Xml_Root, "command") & """is not supported."); 
                    end if;
                    
                  end if;
              exception
                when Error : others => 
                  Utility.Print_Exception(Error);
                  String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, Ada.Exceptions.Exception_Message(Error)));
              end;

            end if;

--            if Master_Helper.Aborted.Check_Clients then
--              String'Output(S, "Server aborted");
--              exit;
--            end if;

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