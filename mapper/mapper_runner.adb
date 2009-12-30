with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Utility;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;
with Xml_Helper;

with Ada.Exceptions;
with Mapper_Helper;

with Logger;

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
      begin
        Ada.Text_IO.Put_Line(Response);
        
        declare
          Xml_Tree : Xml.Node_Access := Xml_Parser.Parse(Content => Response);
        begin
          
          if Xml_Helper.Is_Command(Xml_Tree, "new_access_token") then
            Mapper_Helper.Access_Token := Xml.Get_Value(
              Xml.Find_Child_With_Tag(Xml_Tree, "details"),
              "access_token"
            );
          end if;
          
          if Xml_Helper.Is_Command(Xml_Tree, "error") then
            Ada.Exceptions.Raise_Exception(Utility.Initialisation_Failed'Identity, Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "message"));
          end if;
        end;
        
      end;
    exception
      when Utility.Initialisation_Failed =>
        raise;
      when GNAT.Sockets.Socket_Error =>
        Ada.Exceptions.Raise_Exception(Utility.Initialisation_Failed'Identity, "Ada MR Master is unreachable");
      when Error : others =>
        Utility.Print_Exception(Error);
    end;
    
    -- Ask for new jobs and work off them
    loop
      exit when Mapper_Helper.Aborted.Get_Abort;
      exit when Mapper_Helper.Aborted.Get_Exit; 
      
      
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
          
          Xml_Tree : Xml.Node_Access := Xml_Parser.Parse(Content => Response);
          
          Details_For_Master_Notification : Utility.String_String_Maps.Map;
        begin
          
          if Xml_Helper.Is_Command(Xml_Tree, "new_job") then
            
            declare
              Job : My_Job := From_Xml(Xml.Find_Child_With_Tag(Xml_Tree, "details"));
            begin
              Compute_Job(Job);
              
              -- Send result to reducer
              declare
                Xml_Command : String := Xml_Helper.Xml_Command(
                  G_T     => Xml_Helper.Mapper, 
                  Command => "job_result",
                  Details => Job_Result_To_Xml
                );
              begin
                Logger.Put_Line("Delivering job result to reducer", Logger.Info);
                declare
                  Response : String := Utility.Send(
                    Mapper_Helper.Reducer_Ip,
                    Mapper_Helper.Reducer_Port,
                    Xml_Command,
                    5
                  );
                begin
                  -- if successful, send new job status to master
                  null;
                end;
              exception
                when Error : others =>
                  Logger.Put_Line("Reducer unreachable! Job failed!", Logger.Err);
                  Details_For_Master_Notification.Insert("message", "Reducer (" & GNAT.Sockets.Image(Mapper_Helper.Reducer_Ip) & ":" & Mapper_Helper.Reducer_Port'Img & ") unreachable");
                  
                  --TODO: Send failed query to master!
              end;
              
              Details_For_Master_Notification.Insert("job_id", Get_Job_Id(Job)'Img);
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
              Time : Float := Float'Value(Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "seconds"));
            begin
              Logger.Put_Line("No further job found. Waiting " & Float'Image(Time) & " seconds until next job request!", Logger.Info);
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