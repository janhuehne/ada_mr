-- System libs
with Ada.Exceptions;
with Ada.Text_IO;

-- Project libs
with Xml;
with Xml_Parser;
with Xml_Helper;

with Utility;

with Logger;

package body Master_Server is 
  
  function Exit_Server return Boolean is
  begin
    if Master_Helper.Aborted.Get_Exit = true OR Master_Helper.Aborted.Get_Abort = true then
      return true;
    end if;
      
    return false;
  end Exit_Server;
  
  
  procedure Process_Incomming_Connection(New_Sock : GNAT.Sockets.Socket_Type) is
    Slave : Echo_MR.Echo_Access;
  begin
    Slave := new Echo_MR.Echo;
    Slave.Start(New_Sock);    
  end Process_Incomming_Connection;
  

  procedure Process_Request(S : Stream_Access) is
    Request  : String          := String'Input(S);
    Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Request);
  begin
    if Xml_Helper.Is_Mapper_Request(Xml_Root) or Xml_Helper.Is_Reducer_Request(Xml_Root) then
      
      if Xml_Helper.Is_Command(Xml_Root, "initialization") then
        
        declare
          Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "details");
          Worker_Entry : Master_Helper.Worker_Record_Access := new Master_Helper.Worker_Record;
        begin
          Worker_Entry.Identifier  := ASU.To_Unbounded_String(Xml.Get_Value(Details, "identifier"));
          Worker_Entry.W_Type      := Master_Helper.String_To_Worker_Type(Xml.Get_Value(Details, "type"));
          Worker_Entry.Ip          := Inet_Addr(Xml.Get_Value(Details, "ip"));
          Worker_Entry.Port        := Port_Type'Value(Xml.Get_Value(Details, "port"));
          
          Add_Worker(Worker_Entry);
          
          String'Output(S, Xml_Helper.Xml_Command(
            G_T     => Xml_Helper.Master,
            Command => "new_access_token",
            Details => "<access_token>" & Worker_Entry.Access_Token & "</access_token>"
          ));
          
          Logger.Put_Line("New worker """ & ASU.To_String(Worker_Entry.Identifier) & """ initialized with access token: " & Worker_Entry.Access_Token, Logger.Info);
        exception
          when Error : others =>
            Xml_Helper.Send_Error(S, Xml_Helper.Master, Error);
        end;
      end if;
    end if;
    
    if Xml_Helper.Is_Mapper_Request(Xml_Root) then
      if Xml_Helper.Is_Command(Xml_Root, "job_request") then
        
        declare
          Worker : Master_Helper.Worker_Record_Access;
        begin
          Worker := Find_Worker_By_Access_Token_And_Type(
            Xml.Get_Value(Xml_Root, "access_token"),
            Master_Helper.Mapper
          );
          Logger.Put_Line(ASU.To_String(Worker.Identifier) & ": Job request", Logger.Info);
          
          declare
            Job_Entry : Job_Entry_Record_Access;
          begin
            Job_Entry := Get_Next_Pending_Job;
            
            String'Output(S, Xml_Helper.Xml_Command(
              G_T     => Xml_Helper.Master, 
              Command => "new_job", 
              Details => Job_Entry_To_Xml(Job_Entry)
            ));
          exception
            when Master_Helper.No_Job_Found =>
              String'Output(S, Xml_Helper.Xml_Command(
                G_T     => Xml_Helper.Master, 
                Command => "sleep", 
                Details => "<seconds>10</seconds>"
              ));
            when Error : others => 
              Utility.Print_Exception(Error);
              --Change_Job_State(Job_Entry, Master_Helper.Pending);
          end;
        exception 
          when Error : Master_Helper.No_Worker_Found =>
            Xml_Helper.Send_Error(S, Xml_Helper.Master, Error);
        end;
      
      elsif Xml_Helper.Is_Command(Xml_Root, "change_job_state") then
        declare
          Worker : Master_Helper.Worker_Record_Access;
        begin
          Worker := Find_Worker_By_Access_Token_And_Type(
            Xml.Get_Value(Xml_Root, "access_token"),
            Master_Helper.Mapper
          );
          
          declare
            Details   : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "details");
            Job_Entry : Job_Entry_Record_Access := Get_Job_By_Id(Integer'Value(Xml.Get_Value(Details, "job_id")));
            Job_State : Master_Helper.Job_State := Master_Helper.From_String(Xml.Get_Value(Details, "job_state"));
            Message   : String := Xml.Get_Value_Or_Empty(Details, "message");
          begin
            
            Change_Job_State(Job_Entry, Job_State, Message);
            
            Logger.Put_Line(ASU.To_String(Worker.Identifier) & ": Job state changed (Job_Id: " & Xml.Get_Value(Details, "job_id") & "; State: " & Xml.Get_Value(Details, "job_state") & "; Message: " & Message & ")", Logger.Info);
            
            String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, "okay"));
          end;
        exception 
          when Error : Master_Helper.No_Worker_Found =>
            Xml_Helper.Send_Error(S, Xml_Helper.Master, Error);
        end;
      
      elsif Xml_Helper.Is_Command(Xml_Root, "job_done") then
        declare
          Worker : Master_Helper.Worker_Record_Access;
        begin
          Worker := Find_Worker_By_Access_Token_And_Type(
            Xml.Get_Value(Xml_Root, "access_token"),
            Master_Helper.Mapper
          );
          Logger.Put_Line(ASU.To_String(Worker.Identifier) & ": Job done", Logger.Info);
          
          declare
            Details   : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "details");
            Job_Entry : Job_Entry_Record_Access := Get_Job_By_Id(Integer'Value(Xml.Get_Value(Details, "job_id")));
          begin
            Change_Job_State(Job_Entry, Master_Helper.Done);
            String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, "okay"));
          end;
        exception 
          when Error : Master_Helper.No_Worker_Found =>
            Xml_Helper.Send_Error(S, Xml_Helper.Master, Error);
        end;
            
      else
        Ada.Exceptions.Raise_Exception(Utility.Unknown_Command'Identity, "The command """ & Xml.Get_Value(Xml_Root, "command") & """is not supported."); 
      end if;
      
    end if;
  exception
    when Error : others => 
      Utility.Print_Exception(Error);
      String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
  
end Master_Server;