-- System libs
with Ada.Exceptions;
with Ada.Text_IO;

-- Project libs
with Xml;
with Xml_Parser;
with Xml_Helper;

with Utility;

package body Mapper_Server is 
  
  function Exit_Server return Boolean is
  begin
    return false;
  end Exit_Server;
  
  
  procedure Process_Incomming_Connection(New_Sock : GNAT.Sockets.Socket_Type) is
--    Slave : Echo_MR.Echo_Access;
  begin
    Ada.Text_IO.Put_Line ( "-> New incomming task" );
    
--    Slave := new Echo_MR.Echo;
--    Slave.Start(New_Sock);
    
  end Process_Incomming_Connection;
  

  procedure Process_Request(S : Stream_Access) is
    Request  : String          := String'Input(S);
    Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Request);
  begin
    
    Ada.Text_IO.Put_Line("XML Request: " & Request);
    
  exception
    when Error : others => 
      Utility.Print_Exception(Error);
      String'Output(S, Xml_Helper.Create_System_Control(Xml_Helper.Master, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
end Mapper_Server;