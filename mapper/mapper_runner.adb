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

package body Mapper_Runner is 
  
  task body Runner_Task is
  begin
    loop
      select
        accept Start;
        
        declare
        begin
          Ada.Text_IO.Put_Line("Runner Task started!");
          
          -- Initialization
          declare
          begin
            
            declare
              Response : String := Utility.Send(
                Mapper_Helper.Master_Ip,
                Mapper_Helper.Master_Port,
                Xml_Helper.Create_Initialization(Xml_Helper.Mapper, ASU.To_String(Mapper_Helper.Identifier), Mapper_Helper.Server_Bind_Ip, Mapper_Helper.Server_Bind_Port)
              );
            begin
              Ada.Text_IO.Put_Line(Response);
              
              declare
                Xml_Tree : Xml.Node_Access := Xml_Parser.Parse(Content => Response);
              begin
                Mapper_Helper.Access_Token := Xml.Get_Value(Xml_Tree, "access_token");
                
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
                  Xml_Helper.Create_Job_Request
                );
                
                Xml_Tree : Xml.Node_Access := Xml_Parser.Parse(Content => Response);
              begin
                
                if Xml_Helper.Is_Command(Xml_Tree, "new_job") then
                  
                  declare
                    Job : My_Job := From_Xml(Xml.Find_Child_With_Tag(Xml_Tree, "details"));
                  begin
                    Compute_Job(Job);
                    
                    -- Send result to reducer
                    declare
                    begin
                      declare
                        Response : String := Utility.Send(
                          Mapper_Helper.Reducer_Ip,
                          Mapper_Helper.Reducer_Port,
                          Xml_Helper.Xml_Command(Xml_Helper.Mapper, "job_result", Job_Result_To_Xml)
                        );
                      begin
                        Ada.Text_IO.Put_Line(Response);
                      end;
                    exception
                      when Error : others =>
                        Ada.Text_IO.Put_Line("-> ERROR: Reducer unreachable!");
                        Utility.Print_Exception(Error);
                    end;
                    
                    
                    -- Send job state to master
                    declare
                    begin
                      declare
                        Response : String := Utility.Send(
                          Mapper_Helper.Master_Ip,
                          Mapper_Helper.Master_Port,
                          Xml_Helper.Xml_Command(Xml_Helper.Mapper, "job_done", To_Xml(Job))
                        );
                      begin
                        Ada.Text_IO.Put_Line(Response);
                      end;
                    exception
                      when Error : others =>
                        Ada.Text_IO.Put_Line("-> ERROR: Master unreachable!");
                        Utility.Print_Exception(Error);
                    end;
                     
                  end;
                  
                elsif Xml_Helper.Is_Command(Xml_Tree, "sleep") then
                  
                  declare
                    Time : Float := Float'Value(Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "seconds"));
                  begin
                    Ada.Text_IO.Put_Line("No further job found. Waiting " & Float'Image(Time) & " seconds until next job request!");
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
                Ada.Text_IO.Put_Line("ERROR: Could not parse incomming XML file.");
              
              when Error : Utility.Unknown_Command =>
                Ada.Text_IO.Put_Line("ERROR: Unsupported command: " & Ada.Exceptions.Exception_Message(Error));
              
              when Error : others =>
                Utility.Print_Exception(Error);
            end;
            
          end loop;
        exception
          when Error : others =>
            Ada.Text_IO.Put_Line ("FATAL ERROR: Aborting! Please close the program and check your configuration." );
            Mapper_Helper.Aborted.Set_Abort;
            Utility.Print_Exception(Error);
        end; 
      or
        accept Stop;
        Ada.Text_IO.Put_Line("Stopping Runner");
        exit;
      end select;
    end loop;
  end Runner_Task;
    
end Mapper_Runner;