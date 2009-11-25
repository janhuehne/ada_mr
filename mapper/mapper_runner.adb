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
                exit;
              
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
      
      or
        accept Stop;
        exit;
      end select;
    end loop;
        
        
        
        
        
        
--         Initialize;
--         Create_Socket(Sock);
--         Addr.Addr := Addresses(Get_Host_By_Name ("127.0.0.1"), 1);
--         Addr.Port := 7000;
--         
--         Create_Selector(Read_Selector);
--         Empty(Read_Set);
--         Empty(WSet);
--         
--         Connect_Socket(Sock, Addr);
--         S := Stream (Sock);
--         Boolean'Read (S, B);
--         
--         
--         Set(Read_Set, Sock);
--         
--         -- check for input on socket (server may be aborting)
--         -- time-out immediately if no input pending
--         -- We seem to need a small delay here (using zero seems to block
--         -- forever)
--         -- Is this a GNAT bug or AB misreading Check_Selector docs?
--         Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
--         
--         String'Output(
--           S, 
--           Xml_Helper.Create_Initialization(Xml_Helper.Mapper, "Mapper_01")
--         );
--         
--         declare
--           Str : String := String'Input(S);
--         begin
--           Ada.Text_IO.Put_Line(Str);
--         end;
--         
--         
--         loop 
--           exit when Aborted.Check = true;
--           
--           Set(Read_Set, Sock);
--           
--           -- check for input on socket (server may be aborting)
--           -- time-out immediately if no input pending
--           -- We seem to need a small delay here (using zero seems to block
--           -- forever)
--           -- Is this a GNAT bug or AB misreading Check_Selector docs?
--           Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
--           
--           if Read_Status = Expired then
--             
--             -- ask for new job
--             String'Output(
--               S, 
--               Xml_Helper.Create_Job_Request
--             );
--             
-- --            Ada.Text_IO.Put_Line("Server Request: " & Xml_Helper.Create_Job_Request);
--           
--             declare
--               -- receive message
--               Str : String := String'Input(S);
--             begin
-- --              Ada.Text_IO.Put_Line("Server Response: " & Str);
--               
-- --              if Xml_Helper.Is_Valid_Xml_String(Str) then
--                 
--                 declare
--                   Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
--                 begin
--                   if Utility.Is_Equal(Xml.Get_Tag(Xml_Root), "adamr-master") then
--                       Ada.Text_IO.Put_Line("Unknown command!");
--                     end if;
--                   end if;
--                 end;
-- --              else
-- --                Ada.Text_IO.Put_Line("Not a valid xml response!");
-- --              end if;
--               
--               exit when Str = "Server aborted";
--             end;
--             
--           end if;
--           
--           Ada.Text_IO.New_Line;
--           
--         end loop;
--         
--         --  tidy up 
--         ShutDown_Socket(Sock);
--         Close_Selector(Read_Selector);
--         Finalize;
--         
--         Ada.Text_IO.New_Line;
--         Ada.Text_IO.Put_Line("Mapper task exiting ...");
--         Finalize;
--         exit;
--       end select;
--     end loop;
  exception 
    when Error : others =>
      Ada.Text_IO.Put_Line ("Exception: Client quitting ..." ) ;
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
      
--      Close_Socket(Sock);
--      Close_Selector(Read_Selector);
--      Finalize;
  end Runner_Task;
    
end Mapper_Runner;