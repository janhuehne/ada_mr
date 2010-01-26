-- System libs
with Ada.Exceptions;
with Ada.Text_IO;

-- Project libs
with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Mapper.Helper;
with Ada_Mr.Logger;

package body Ada_Mr.Mapper.Server is 
  
  function Exit_Server return Boolean is
  begin
    return Ada_Mr.Mapper.Helper.Aborted.Check;
  end Exit_Server;
  
  
  procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access) is
    use Ada_Mr.Helper;
  begin
    if From = Master then
      
      if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "exit") then
        
        declare
          Details : Ada_Mr.Xml.Node_Access;
        begin
          Details := Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "details");
          
          if Boolean'Value(Ada_Mr.Xml.Get_Value(Details, "abort")) = true then
            Abort_Mapper;
          else
            Stop_Mapper;
          end if;
          
        exception
          when others => Stop_Mapper;
        end;
        
        String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Mapper, "okay"));
      end if;
    end if;
      
  exception
    when Error : others => 
      Ada_Mr.Helper.Print_Exception(Error);
      String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Master, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
end Ada_Mr.Mapper.Server;