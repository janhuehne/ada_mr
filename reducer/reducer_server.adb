-- System libs
with Ada.Exceptions;
with Ada.Text_IO;

-- Project libs
with Xml;
with Xml_Parser;
with Xml_Helper;

with Application_Helper;

package body Reducer_Server is 
  
  function Exit_Server return Boolean is
  begin
    return Reducer_Helper.Aborted.Check;
  end Exit_Server;
  
  
  procedure Process_Incomming_Connection(New_Sock : GNAT.Sockets.Socket_Type) is
    Slave : Echo_MR.Echo_Access;
  begin
    Ada.Text_IO.Put_Line ( "-> New incomming task" );
    
    Slave := new Echo_MR.Echo;
    Slave.Start(New_Sock);
    
  end Process_Incomming_Connection;
  

  procedure Process_Request(S : Stream_Access; From : Application_Helper.Worker_Type; Xml_Root : Xml.Node_Access) is
  begin
    if Application_Helper."="(From, Application_Helper.Mapper) then
      
      if Xml_Helper.Is_Command(Xml_Root, "job_result") then
      
        Reducer_Helper.Finished_Jobs_Queue.Append(Xml.Find_Child_With_Tag(Xml_Root, "details"));
        
        String'Output(
          S, 
          Xml_Helper.Create_System_Control(Xml_Helper.Reducer, "okay")
        );
      end if;
        
    elsif Application_Helper."="(From, Application_Helper.Master) then
      
      if Xml_Helper.Is_Command(Xml_Root, "finalize") then
        Finalize_Jobs;
        
        String'Output(
          S, 
          Xml_Helper.Create_System_Control(Xml_Helper.Reducer, "okay")
        );
      end if;
      
    end if;
    
  exception
    when Error : others => 
      Application_Helper.Print_Exception(Error);
      String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Reducer, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
end Reducer_Server;