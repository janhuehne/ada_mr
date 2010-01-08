with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Application_Helper;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;
with Xml_Helper;

with Ada.Exceptions;
with Reducer_Helper;

with Logger;

package body Reducer_Runner is 
  
  procedure Run is
  begin
    -- Initialization
    for I in 1 .. 10 loop
      declare
      begin
        
        declare
          Response : String := Application_Helper.Send(
            Reducer_Helper.Master_Ip,
            Reducer_Helper.Master_Port,
            Xml_Helper.Create_Initialization(Xml_Helper.Reducer, ASU.To_String(Reducer_Helper.Identifier), Reducer_Helper.Server_Bind_Ip, Reducer_Helper.Server_Bind_Port)
          );
        begin
          declare
            Xml_Tree : Xml.Node_Access;
          begin
            Xml_Tree := Xml_Helper.Get_Verified_Content(Xml_Parser.Parse(Content => Response));
            if Xml_Helper.Is_Command(Xml_Tree, "new_access_token") then
              Reducer_Helper.Access_Token := Xml.Get_Value(
                Xml.Find_Child_With_Tag(Xml_Tree, "details"),
                "access_token"
              );
            end if;
            
            if Xml_Helper.Is_Command(Xml_Tree, "error") then
              Ada.Exceptions.Raise_Exception(Application_Helper.Initialisation_Failed'Identity, Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "message"));
            end if;
          end;
          
        end;
        
        exit;
        
      exception
        when Application_Helper.Initialisation_Failed =>
          raise;
        when GNAT.Sockets.Socket_Error =>
          Logger.Put_Line("Attempt " & I'Img & ": Master server is unreachable. Trying again.", Logger.Warn);
          
          if I = 10 then
            Ada.Exceptions.Raise_Exception(Application_Helper.Initialisation_Failed'Identity, "Ada MR Master is unreachable");
          end if;
        when Error : others =>
          Application_Helper.Print_Exception(Error);
      end;
    end loop;
    
  exception
    when Error : others =>
      Application_Helper.Print_Exception(Error);
      Reducer_Helper.Aborted.Stop;
  end Run; 
  
end Reducer_Runner;