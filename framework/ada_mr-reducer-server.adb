-- System libs
with Ada.Exceptions;
with Ada.Text_IO;

-- Project libs
with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;

with Ada_Mr.Helper;

package body Ada_Mr.Reducer.Server is 
  
  function Exit_Server return Boolean is
  begin
    return Ada_Mr.Reducer.Helper.Aborted.Check;
  end Exit_Server;
  
  
  
  procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access) is
  begin
    if Ada_Mr.Helper."="(From, Ada_Mr.Helper.Mapper) then
      
      if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "job_result") then
      
        Ada_Mr.Reducer.Helper.Finished_Jobs_Queue.Append(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "details"));
        
        String'Output(
          S, 
          Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Reducer, "okay")
        );
      end if;
        
    elsif Ada_Mr.Helper."="(From, Ada_Mr.Helper.Master) then
      
      if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "finalize") then
        Finalize_Jobs;
        
        String'Output(
          S, 
          Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Reducer, "okay")
        );
      end if;
      
    end if;
    
  exception
    when Error : others => 
      Ada_Mr.Helper.Print_Exception(Error);
      String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Reducer, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
end Ada_Mr.Reducer.Server;