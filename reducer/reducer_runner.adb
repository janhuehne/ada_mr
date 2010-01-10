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
    Master_Ip   : GNAT.Sockets.Inet_Addr_Type;
    Master_Port : GNAT.Sockets.Port_Type;
  begin
    Master_Ip   := GNAT.Sockets.Inet_Addr(Application_Helper.Read_Configuration("MASTER-IP"));
    Master_Port := GNAT.Sockets.Port_Type'Value(Application_Helper.Read_Configuration("MASTER-PORT"));
    
    -- Initialization
    begin
      declare
        Response : String := Application_Helper.Send(
          Master_Ip,
          Master_Port,
          Xml_Helper.Create_Initialization(
            Xml_Helper.Reducer, 
            Application_Helper.Read_Configuration("IDENTIFIER"), 
            GNAT.Sockets.Inet_Addr(Application_Helper.Read_Configuration("LOCAL_SERVER", "BIND_IP")),
            GNAT.Sockets.Port_Type'Value(Application_Helper.Read_Configuration("LOCAL_SERVER", "BIND_PORT"))
          ),
          Natural'Value(Application_Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
          Natural'Value(Application_Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
        );
        
        Xml_Tree : Xml.Node_Access;
      begin
        Xml_Tree := Xml_Helper.Get_Verified_Content(Xml_Parser.Parse(Content => Response));
        
        if Xml_Helper.Is_Command(Xml_Tree, "new_access_token") then
          Application_Helper.Add_Configuration(
            "ACCESS_TOKEN", 
            Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "access_token")
          );
                    
          Logger.Put_Line("Reducer initalized with access token """ & Application_Helper.Read_Configuration("ACCESS_TOKEN") & """", Logger.Info);
        end if;
        
        if Xml_Helper.Is_Command(Xml_Tree, "error") then
          Ada.Exceptions.Raise_Exception(Application_Helper.Initialisation_Failed'Identity, Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Tree, "details"), "message"));
        end if;
      end;
    exception
      when Application_Helper.Initialisation_Failed =>
        raise;
      when GNAT.Sockets.Socket_Error =>
        Ada.Exceptions.Raise_Exception(Application_Helper.Initialisation_Failed'Identity, "Ada MR Master is not reachable");
      when Error : others =>
        Application_Helper.Print_Exception(Error);
    end;
    
  exception
    when Error : others =>
      Application_Helper.Print_Exception(Error);
      Stop_Reducer;
  end Run; 
  
end Reducer_Runner;