with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Ada_Mr.Helper;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;

with Ada.Exceptions;
with Ada_Mr.Reducer.Helper;

with Ada_Mr.Logger;

package body Ada_Mr.Reducer.Runner is 
  
  procedure Run is
    Master_Ip   : GNAT.Sockets.Inet_Addr_Type;
    Master_Port : GNAT.Sockets.Port_Type;
  begin
    Master_Ip   := GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("MASTER-IP"));
    Master_Port := GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("MASTER-PORT"));
    
    -- Initialization
    begin
      declare
        Response : String := Ada_Mr.Helper.Send(
          Master_Ip,
          Master_Port,
          Ada_Mr.Xml.Helper.Create_Initialization(
            Ada_Mr.Xml.Helper.Reducer, 
            Ada_Mr.Helper.Read_Configuration_Or_Null("IDENTIFIER"), 
            GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "IP")),
            GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "PORT"))
          ),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
        );
        
        Xml_Tree : Ada_Mr.Xml.Node_Access;
      begin
        Xml_Tree := Ada_Mr.Xml.Helper.Get_Verified_Content(Ada_Mr.Xml.Parser.Parse(Content => Response));
        
        if Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "new_access_token") then
          Ada_Mr.Helper.Add_Configuration(
            "ACCESS_TOKEN", 
            Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details"), "access_token")
          );
                    
          Ada_Mr.Logger.Put_Line("Reducer initalized with access token """ & Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN") & """", Ada_Mr.Logger.Info);
        end if;
        
        if Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "error") then
          Ada.Exceptions.Raise_Exception(Ada_Mr.Helper.Initialisation_Failed'Identity, Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details"), "message"));
        end if;
      end;
    exception
      when Ada_Mr.Helper.Initialisation_Failed =>
        raise;
      when GNAT.Sockets.Socket_Error =>
        Ada.Exceptions.Raise_Exception(Ada_Mr.Helper.Initialisation_Failed'Identity, "Ada MR Master is not reachable");
      when Error : others =>
        Ada_Mr.Helper.Print_Exception(Error);
        raise;
    end;
    
    -- Ask master for not delivered mapper results
    Ada_Mr.Reducer.Helper.Import_Not_Delivered_Mapper_Results_From_Master;
    
    
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      Stop_Reducer;
  end Run; 
  
end Ada_Mr.Reducer.Runner;