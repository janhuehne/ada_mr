-- System libs
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Calendar;

-- Project libs
with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;

with Ada_Mr.Helper;

with Ada_Mr.Logger;

with Ada_Mr.Crypt.Helper;

package body Ada_Mr.Master.Server is 
  
  function Exit_Server return Boolean is
  begin
    if Ada_Mr.Master.Helper.Aborted.Get_Exit = true OR Ada_Mr.Master.Helper.Aborted.Get_Abort = true then
      return true;
    end if;
      
    return false;
  end Exit_Server;
  
  
  procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access) is
    use Ada_Mr.Helper;
  begin
    
    if From = Mapper or From = Reducer then
      
      if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "initialization") then
        
        Ada_Mr.Logger.Put_Line(GNAT.Sockets.Image(GNAT.Sockets.Get_Address(New_Sock).Addr), Ada_Mr.Logger.Err);
        
        
        declare
          Details : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "details");
          Worker_Entry : Ada_Mr.Master.Helper.Worker_Record_Access := new Ada_Mr.Master.Helper.Worker_Record;
        begin
          Worker_Entry.Identifier  := ASU.To_Unbounded_String(Ada_Mr.Xml.Get_Value(Details, "identifier"));
          Worker_Entry.W_Type      := Ada_Mr.Helper.String_To_Worker_Type(Ada_Mr.Xml.Get_Value(Details, "type"));
          Worker_Entry.Ip          := Inet_Addr(Ada_Mr.Xml.Get_Value(Details, "ip"));
          Worker_Entry.Port        := Port_Type'Value(Ada_Mr.Xml.Get_Value(Details, "port"));
          
          Add_Worker(Worker_Entry);
          
          String'Output(S, Ada_Mr.Xml.Helper.Xml_Command(
            G_T     => Ada_Mr.Xml.Helper.Master,
            Command => "new_access_token",
            Details => "<identifier>" & ASU.To_String(Worker_Entry.Identifier) & "</identifier><access_token>" & Worker_Entry.Access_Token & "</access_token>"
          ));
          
          Ada_Mr.Logger.Put_Line("New worker """ & ASU.To_String(Worker_Entry.Identifier) & """ initialized with access token: " & Worker_Entry.Access_Token, Ada_Mr.Logger.Info);
        exception
          when Error : others =>
            Ada_Mr.Xml.Helper.Send_Error(S, Ada_Mr.Xml.Helper.Master, Error);
        end;
      
      else
        
        declare
          Worker : Ada_Mr.Master.Helper.Worker_Record_Access;
        begin
          Worker := Find_Worker_By_Access_Token_And_Type(
            Ada_Mr.Xml.Get_Value(Xml_Root, "access_token"),
            From
          );
          
          
          -- <-- Start Handle Mapper requests
          if From = Mapper then
            
            
            -- *******************************************************
            -- New job request
            if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "job_request") then
              
              Ada_Mr.Logger.Put_Line(ASU.To_String(Worker.Identifier) & ": Job request", Ada_Mr.Logger.Info);
              
              declare
                Job_Entry : Job_Entry_Record_Access;
              begin
                Job_Entry := Get_Next_Pending_Job;
                
                String'Output(S, Ada_Mr.Xml.Helper.Xml_Command(
                  G_T     => Ada_Mr.Xml.Helper.Master, 
                  Command => "new_job", 
                  Details => Job_Entry_To_Xml(Job_Entry)
                ));
              exception
                when Ada_Mr.Master.Helper.No_Job_Found =>
                  String'Output(S, Ada_Mr.Xml.Helper.Xml_Command(
                    G_T     => Ada_Mr.Xml.Helper.Master, 
                    Command => "sleep", 
                    Details => "<seconds>10</seconds>"
                  ));
                when Error : others => 
                  Ada_Mr.Helper.Print_Exception(Error);
              end;
            
            
            -- *******************************************************
            -- Change job state
            elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "change_job_state") then
              
              declare
                Details   : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "details");
                Job_Entry : Job_Entry_Record_Access := Get_Job_By_Id(Integer'Value(Ada_Mr.Xml.Get_Value(Details, "job_id")));
                Job_State : Ada_Mr.Master.Helper.Job_State := Ada_Mr.Master.Helper.From_String(Ada_Mr.Xml.Get_Value(Details, "job_state"));
                Message   : String := Ada_Mr.Xml.Get_Value_Or_Empty(Details, "message");
              begin
                Change_Job_State(Job_Entry, Job_State, Message);
                
                Ada_Mr.Logger.Put_Line(ASU.To_String(Worker.Identifier) & ": Job state changed (Job_Id: " & Ada_Mr.Xml.Get_Value(Details, "job_id") & "; State: " & Ada_Mr.Xml.Get_Value(Details, "job_state") & "; Message: " & Message & ")", Ada_Mr.Logger.Info);
                
                String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, "okay"));
              end;
              
            -- *******************************************************
            -- Store not delivered map result (e.g. a reducer is not reachable)
            elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "not_delivered_map_result") then
              
              declare
                Not_Delivered_Map_Result : Ada_Mr.Master.Helper.Not_Delivered_Map_Result_Access := new Ada_Mr.Master.Helper.Not_Delivered_Map_Result;
              begin
                Not_Delivered_Map_Result.Reducer := Worker;
                Not_Delivered_Map_Result.Result  := ASU.To_Unbounded_String("aa");
                
                Ada_Mr.Master.Helper.Undelivered_Job_Results.Append(Not_Delivered_Map_Result);
                Ada_Mr.Logger.Put_Line(ASU.To_String(Worker.Identifier) & ": A job result could not delivered to a reducer.", Ada_Mr.Logger.Warn);
              end;
              
              String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, "okay"));
            
              -- *******************************************************
              -- Return the reducer details like ip and port
            elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "reducer_details") then
              
              declare
                Details : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "details");
                Reducer : Ada_Mr.Master.Helper.Worker_Record_Access;
              begin
                Reducer := Find_Worker_By_Identifier(Ada_Mr.Xml.Get_Value(Details, "identifier"));
                
                String'Output(S, Ada_Mr.Xml.Helper.Xml_Command(
                  G_T     => Ada_Mr.Xml.Helper.Master, 
                  Command => "reducer_details", 
                  Details => "<ip>" & Ada_Mr.Helper.Trim(GNAT.Sockets.Image(Reducer.Ip)) & "</ip><port>" & Ada_Mr.Helper.Trim(Reducer.Port'Img) & "</port>"
                ));
              exception
                when Error : Ada_Mr.Master.Helper.No_Worker_Found =>
                  Ada_Mr.Xml.Helper.Send_Error(S, Ada_Mr.Xml.Helper.Master, Error);
              end;
            elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "ping") then
              Worker.Updated_At := Ada.Calendar.Clock;
              String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, "okay"));
            else
              Ada.Exceptions.Raise_Exception(Ada_Mr.Helper.Unknown_Command'Identity, "The command """ & Ada_Mr.Xml.Get_Value(Xml_Root, "command") & """is not supported from a mapper.");
            end if;
          
          elsif From = Reducer then
            
            if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "stop_map_reduce_system") then
              Ada_Mr.Master.Helper.Stop_Map_Reduce_System := True;
              String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, "okay"));
            end if;
          
          end if;
          -- End Handle Mapper requests -->
          
        exception
          when Error : Ada_Mr.Master.Helper.No_Worker_Found =>
            Ada_Mr.Xml.Helper.Send_Error(S, Ada_Mr.Xml.Helper.Master, Error);
        end;
        
      end if;
      
    end if;
 
  exception
    when Error : others => 
      Ada_Mr.Helper.Print_Exception(Error);
      String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
  
end Ada_Mr.Master.Server;