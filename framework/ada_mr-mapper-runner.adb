--with Ada_Mr.Helper.Worker_Type.Text_IO;
--with Ada_Mr.Helper.Worker_Type.Strings.Unbounded;
--with Ada_Mr.Helper.Worker_Type.IO_Exceptions;
--
--with Ada_Mr.Helper.Worker_Type.Characters.Handling;
--use Ada_Mr.Helper.Worker_Type.Characters.Handling;
with Ada.Exceptions;

with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;

--with Ada_Mr.Helper.Worker_Type.Exceptions;
with Ada_Mr.Mapper.Helper;

with Ada_Mr.Logger;
with Ada_Mr.Helper;

package body Ada_Mr.Mapper.Runner is 
  
  procedure Run is
    Master_Ip   : GNAT.Sockets.Inet_Addr_Type;
    Master_Port : GNAT.Sockets.Port_Type;
  begin
    Master_Ip   := GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("MASTER-IP"));
    Master_Port := GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("MASTER-PORT"));

    -- Initialization
    begin
      declare
        Response : String := Ada_Mr.Helper.Send(
          Master_Ip,
          Master_Port,
          Ada_Mr.Xml.Helper.Create_Initialization(
            Ada_Mr.Xml.Helper.Mapper, 
            Ada_Mr.Helper.Read_Configuration_Or_Null("IDENTIFIER"), 
            GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "BIND_IP")),
            GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "BIND_PORT"))
          ),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
        );
        
        Xml_Tree : Ada_Mr.Xml.Node_Access;
      begin
        Xml_Tree := Ada_Mr.Xml.Helper.Get_Verified_Content(Ada_Mr.Xml.Parser.Parse(Content => Response));
        
        if Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "new_access_token") then
          Ada_Mr.Helper.Add_Configuration(
            "ACCESS_TOKEN", 
            Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details"), "access_token")
          );
                    
          Ada_Mr.Logger.Put_Line("Mapper initalized with access token """ & Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN") & """", Ada_Mr.Logger.Info);
        end if;
        
        if Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "error") then
          Ada.Exceptions.Raise_Exception(Ada_Mr.Helper.Initialisation_Failed'Identity, Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details"), "message"));
        end if;
      end;
    exception
      when Ada_Mr.Helper.Initialisation_Failed =>
        raise;
      when GNAT.Sockets.Socket_Error =>
        Ada.Exceptions.Raise_Exception(Ada_Mr.Helper.Initialisation_Failed'Identity, "Ada MR Master is not reachable");
      when Error : others =>
        Ada_Mr.Helper.Print_Exception(Error);
    end;
    
    -- Ask for new jobs and work off them
    loop
      exit when Ada_Mr.Mapper.Helper.Aborted.Check;
      
      
      Ada_Mr.Logger.Put_Line("Asking for a new job", Ada_Mr.Logger.Info);
      
      declare
      begin
        
        declare
          Response : String := Ada_Mr.Helper.Send(
            Master_Ip,
            Master_Port,
            Ada_Mr.Xml.Helper.Xml_Command(
              G_T          => Ada_Mr.Xml.Helper.Mapper,
              Command      => "job_request",
              Access_Token => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN")
            ),
            Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
            Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
          );
          
          Xml_Tree : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Helper.Get_Verified_Content(Ada_Mr.Xml.Parser.Parse(Content => Response));
          
          Details_For_Master_Notification : Ada_Mr.Helper.String_String_Maps.Map;
        begin
          
          if Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "new_job") then
            
            Ada_Mr.Logger.Put_Line("New job received", Ada_Mr.Logger.Info);
            
            declare
              Job : My_Job;
            begin
              Job := From_Xml(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details"));
              
              Ada_Mr.Logger.Put_Line("Computing job", Ada_Mr.Logger.Info);
              Compute_Job(Job);
              Ada_Mr.Logger.Put_Line("Job computed", Ada_Mr.Logger.Info);
              
              -- --> Send result to reducers
              Ada_Mr.Mapper.Helper.Send_Result(Split_Result_For_Different_Reducer);
              -- <-- Send result to reducer
              
              -- send new job status to master
              Details_For_Master_Notification.Insert("job_id", Ada_Mr.Helper.Trim(Get_Job_Id(Job)'Img));
              Details_For_Master_Notification.Insert("job_state", "done");
              
              declare
              begin
                declare
                  Response : String := Ada_Mr.Helper.Send(
                    Master_Ip,
                    Master_Port,
                    Ada_Mr.Xml.Helper.Xml_Command(
                      G_T     => Ada_Mr.Xml.Helper.Mapper,
                      Command => "change_job_state",
                      Access_Token => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN"),
                      Details => Details_For_Master_Notification
                    ),
                    Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
                    Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
                  );
                begin
                  null;
                end;
              exception
                when Error : others =>
                  Ada_Mr.Logger.Put_Line("Master unreachable! Job failed!", Ada_Mr.Logger.Err);
              end;
            exception
              when Error : others =>
                Ada_Mr.Helper.Print_Exception(Error);
            end;
            
          elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "sleep") then
            
            declare
              Time : Integer := Integer'Value(Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details"), "seconds"));
            begin
              Ada_Mr.Logger.Put_Line("No further job found. Waiting " & Integer'Image(Time) & " seconds until next job request!", Ada_Mr.Logger.Info);
              delay Duration(Time);
            end;
          
          elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "exit") then
            Ada_Mr.Mapper.Helper.Aborted.Stop;
          
          else
            Ada.Exceptions.Raise_Exception(Ada_Mr.Helper.Unknown_Command'Identity, "Unsupported command: """ & Ada_Mr.Xml.Get_Value(Xml_Tree, "command"));
          end if;
          
        end;
        
      exception
        when Error : Ada_Mr.Xml.Parser.Xml_Parse_Error =>
          Ada_Mr.Logger.Put_Line("Could not parse incomming XML file.", Ada_Mr.Logger.Err);
        
        when Error : Ada_Mr.Helper.Unknown_Command =>
          Ada_Mr.Logger.Put_Line("Unsupported command: " & Ada.Exceptions.Exception_Message(Error), Ada_Mr.Logger.Warn);
        
        when Error : others =>
          Ada_Mr.Helper.Print_Exception(Error);
      end;
      
    end loop;
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      Stop_Mapper;
  end Run; 
  
end Ada_Mr.Mapper.Runner;