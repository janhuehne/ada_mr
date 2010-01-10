-- System libs
with Ada.Exceptions;
with Ada.Text_IO;

-- Project libs
with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Mapper.Helper;

package body Ada_Mr.Mapper.Server is 
  
  function Exit_Server return Boolean is
  begin
    return Ada_Mr.Mapper.Helper.Aborted.Check;
  end Exit_Server;
  
  
  procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access) is
  begin
    null;
  exception
    when Error : others => 
      Ada_Mr.Helper.Print_Exception(Error);
      String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
end Ada_Mr.Mapper.Server;