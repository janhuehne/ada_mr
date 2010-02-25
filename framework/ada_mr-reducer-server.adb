with Ada.Exceptions;
with Ada.Text_IO;
with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Helper;

package body Ada_Mr.Reducer.Server is 
  
  -----------------
  -- Exit_Server --
  -----------------
  function Exit_Server
    return Boolean 
  is
  begin
    return Ada_Mr.Reducer.Helper.Aborted.Check;
  end Exit_Server;
  
  
  
  ---------------------
  -- Process_Request --
  ---------------------
  procedure Process_Request
    (S        : Stream_Access;
     From     : Ada_Mr.Helper.Worker_Type;
     Xml_Root : Ada_Mr.Xml.Node_Access) 
  is
    use Ada_Mr.Helper;
  begin
    if From = Mapper then
      
      if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "job_result") then
      
        Ada_Mr.Reducer.Helper.Mapper_Results.Add(
          Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "details")
        );
        
        String'Output(
          S, 
          Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Reducer, "okay")
        );
      end if;
        
    elsif From = Master then
      
      if Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "finalize") then
        Ada_Mr.Reducer.Helper.Import_Not_Delivered_Mapper_Results_From_Master;
        
        loop
          exit when Ada_Mr.Reducer.Helper.Mapper_Results.Is_Empty = True;
        end loop;
        
        Finalize_Jobs;
        
        String'Output(
          S, 
          Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Reducer, "okay")
        );
      
      elsif Ada_Mr.Xml.Helper.Is_Command(Xml_Root, "exit") then
        String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Mapper, "okay"));
        Stop_Reducer;
      end if;

    end if;
    
  exception
    when Error : others => 
      Ada_Mr.Helper.Print_Exception(Error);
      String'Output(S, Ada_Mr.Xml.Helper.Create_System_Control(Ada_Mr.Xml.Helper.Reducer, Ada.Exceptions.Exception_Message(Error)));
  end Process_Request;
  
end Ada_Mr.Reducer.Server;