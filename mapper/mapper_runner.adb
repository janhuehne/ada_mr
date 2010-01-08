--with Utility.Worker_Type.Text_IO;
--with Utility.Worker_Type.Strings.Unbounded;
--with Utility.Worker_Type.IO_Exceptions;
--
--with Utility.Worker_Type.Characters.Handling;
--use Utility.Worker_Type.Characters.Handling;
with Ada.Exceptions;

with Xml;
with Xml_Parser;
with Xml_Helper;

--with Utility.Worker_Type.Exceptions;
with Mapper_Helper;

with Logger;
with Utility;

package body Mapper_Runner is 
  
  procedure Run is
  begin
    null;
    -- Initialization
    begin
      declare
        Response : String := Utility.Send(
          Mapper_Helper.Master_Ip,
          Mapper_Helper.Master_Port,
          Xml_Helper.Create_Initialization(Xml_Helper.Mapper, ASU.To_String(Mapper_Helper.Identifier), Mapper_Helper.Server_Bind_Ip, Mapper_Helper.Server_Bind_Port),
          10
        );
        
        Xml_Tree : Xml.Node_Access;
      begin
        Xml_Tree := Xml_Helper.Get_Verified_Content(Xml_Parser.Parse(Content => Response));
        
        if Xml_Helper.Is_Command(Xml_Tree, "new_access_token") then
          Mapper_Helper.Access_Token := Xml.Get_Value(
            Xml.Find_Child_With_Tag(Xml_Tree, "details"),
            "access_token"
          );
          
          Logger.Put_Line("Mapper initalized with access token """ & Mapper_Helper.Access_Token & """", Logger.Info);
          
        end if;
        
        if Xml_Helper.Is_Command(Xml_Tree, "error") then
          Ada.Exceptions.Raise_Exception(Utility.Initialisation_Failed'Identity, Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "message"));
        end if;
      end;
    exception
      when Utility.Initialisation_Failed =>
        raise;
      when GNAT.Sockets.Socket_Error =>
        Ada.Exceptions.Raise_Exception(Utility.Initialisation_Failed'Identity, "Utility.Worker_Type MR Master is unreachable");
      when Error : others =>
        Utility.Print_Exception(Error);
    end;
    
    -- Ask for new jobs and work off them
    loop
      exit when Mapper_Helper.Aborted.Get_Abort;
      exit when Mapper_Helper.Aborted.Get_Exit; 
      
      
      Logger.Put_Line("Asking for a new job", Logger.Info);
      
      declare
      begin
        
        declare
          Response : String := Utility.Send(
            Mapper_Helper.Master_Ip,
            Mapper_Helper.Master_Port,
            Xml_Helper.Xml_Command(
              G_T          => Xml_Helper.Mapper,
              Command      => "job_request",
              Access_Token => Mapper_Helper.Access_Token
            )
          );
          
          Xml_Tree : Xml.Node_Access := Xml_Helper.Get_Verified_Content(Xml_Parser.Parse(Content => Response));
          
          Details_For_Master_Notification : Utility.String_String_Maps.Map;
        begin
          
          if Xml_Helper.Is_Command(Xml_Tree, "new_job") then
            
            Logger.Put_Line("New job received", Logger.Info);
            
            declare
              Job : My_Job := From_Xml(Xml.Find_Child_With_Tag(Xml_Tree, "details"));
            begin
              Logger.Put_Line("Computing job", Logger.Info);
              Compute_Job(Job);
              Logger.Put_Line("Job computed", Logger.Info);
              
              -- --> Send result to reducers
              declare
                Reducer_Result_Map : Utility.String_String_Maps.Map;
                
                
                procedure Send_Result_To_Reducer(Cursor : Utility.String_String_Maps.Cursor) is
                  Reducer_Identifier : String := Utility.String_String_Maps.Key(Cursor);
                  Result             : String := Utility.String_String_Maps.Element(Reducer_Result_Map, Reducer_Identifier);
                begin
                  Logger.Put_Line("Sending " & Result & " to " & Reducer_Identifier, Logger.Info);
                  
                  -- Asking master for reducer details
                  declare
                    Xml_Command : String := Xml_Helper.Xml_Command(
                      G_T           => Xml_Helper.Mapper,
                      Command       => "reducer_details",
                      Access_Token  => Mapper_Helper.Access_Token,
                      Details       => "<identifier>" & Reducer_Identifier & "</identifier>"
                    );
                    
                    Response : String := Utility.Send(
                      Mapper_Helper.Master_Ip,
                      Mapper_Helper.Master_Port,
                      Xml_Command,
                      5
                    );
                    
                    Reducer_Details_Xml : Xml.Node_Access;
                  begin
                    Reducer_Details_Xml := Xml_Helper.Get_Verified_Content(Xml_Parser.Parse(Content => Response));
                    
                    if Xml_Helper.Is_Command(Reducer_Details_Xml, "reducer_details") then
                      
                      declare
                        Xml_Command : String := Xml_Helper.Xml_Command(
                          G_T     => Xml_Helper.Mapper, 
                          Command => "job_result",
                          Details => Result
                        );
                        
                        Reducer_Ip   : String := Xml.Get_Value(Xml.Find_Child_With_Tag(Reducer_Details_Xml, "details"), "ip");
                        Reducer_Port : String := Xml.Get_Value(Xml.Find_Child_With_Tag(Reducer_Details_Xml, "details"), "port");
                        
                        Response : String := Utility.Send(
                          Reducer_Ip,
                          Reducer_Port,
                          Xml_Command,
                          5
                        );
                      begin
                        Logger.Put_Line("Job result send to reducer """ & Reducer_Identifier & """", Logger.Info);
                      end;
                    
                    elsif Xml_Helper.Is_Command(Reducer_Details_Xml, "error") then
                      Logger.Put_Line(Xml.Get_Value(Xml.Find_Child_With_Tag(Reducer_Details_Xml, "details"), "message"), Logger.Warn);
                      raise Mapper_Helper.Reducer_Not_Found;
                    end if;
                  
                  exception
                    when Error : others =>
                      Utility.Print_Exception(Error);
                      Logger.Put_Line("Reducer not found or not reachable! Sending job result to the master!", Logger.Err);
                      Details_For_Master_Notification.Insert("message", Reducer_Identifier & "not reachable");
                      
                      declare
                      begin
                        declare
                          Response : String := Utility.Send(
                            Mapper_Helper.Master_Ip,
                            Mapper_Helper.Master_Port,
                            Xml_Helper.Xml_Command(
                              G_T     => Xml_Helper.Mapper,
                              Command => "not_delivered_map_result",
                              Access_Token => Mapper_Helper.Access_Token,
                              Details => "<result>" & Result & "</result>"
                            ),
                            5
                          );
                        begin
                          null;
                        end;
                      exception
                        when Error : others =>
                          Utility.Print_Exception(Error);
                          Logger.Put_Line("Master unreachable! Job failed!", Logger.Err);
                      end;
                  end;
                  
                end Send_Result_To_Reducer;
                
              begin
                Reducer_Result_Map := Split_Result_For_Different_Reducer;
                
                Reducer_Result_Map.Iterate(Send_Result_To_Reducer'Access);
              end;
              -- <-- Send result to reducer
              
              -- send new job status to master
              Details_For_Master_Notification.Insert("job_id", Utility.Trim(Get_Job_Id(Job)'Img));
              Details_For_Master_Notification.Insert("job_state", "done");
              
              declare
              begin
                declare
                  Response : String := Utility.Send(
                    Mapper_Helper.Master_Ip,
                    Mapper_Helper.Master_Port,
                    Xml_Helper.Xml_Command(
                      G_T     => Xml_Helper.Mapper,
                      Command => "change_job_state",
                      Access_Token => Mapper_Helper.Access_Token,
                      Details => Details_For_Master_Notification
                    ),
                    5
                  );
                begin
                  null;
                end;
              exception
                when Error : others =>
                  Logger.Put_Line("Master unreachable! Job failed!", Logger.Err);
              end;
             
            end;
            
          elsif Xml_Helper.Is_Command(Xml_Tree, "sleep") then
            
            declare
              Time : Integer := Integer'Value(Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "seconds"));
            begin
              Logger.Put_Line("No further job found. Waiting " & Integer'Image(Time) & " seconds until next job request!", Logger.Info);
              delay Duration(Time);
            end;
          
          elsif Xml_Helper.Is_Command(Xml_Tree, "exit") then
            Mapper_Helper.Aborted.Set_Exit;
          
          else
            Ada.Exceptions.Raise_Exception(Utility.Unknown_Command'Identity, "Unsupported command: """ & Xml.Get_Value(Xml_Tree, "command"));
            
          end if;
          
        end;
        
      exception
        when Error : Xml_Parser.Xml_Parse_Error =>
          Logger.Put_Line("Could not parse incomming XML file.", Logger.Err);
        
        when Error : Utility.Unknown_Command =>
          Logger.Put_Line("Unsupported command: " & Ada.Exceptions.Exception_Message(Error), Logger.Warn);
        
        when Error : others =>
          Utility.Print_Exception(Error);
      end;
      
    end loop;
  exception
    when Error : others =>
      Utility.Print_Exception(Error);
      Mapper_Helper.Aborted.Set_Abort;
  end Run; 
  
end Mapper_Runner;