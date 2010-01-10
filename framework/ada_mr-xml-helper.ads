with Ada_Mr.Helper;
with Ada.Strings.Unbounded;
with Ada_Mr.Xml;
with GNAT.Sockets;
with Ada.Exceptions;

package Ada_Mr.Xml.Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Group_Tag is (Mapper, Reducer, Master);
  
  function To_String(G_T : Group_Tag) return String;
  
  
  function Xml_Command(G_T : Group_Tag; Command : String; Details : String := "") return String;
  function Xml_Command(G_T : Group_Tag; Command : String; Access_Token : String; Details : String := "") return String;
  function Xml_Command(G_T : Group_Tag; Command : String; Details : Ada_Mr.Helper.String_String_Maps.Map) return String;
  function Xml_Command(G_T : Group_Tag; Command : String; Access_Token : String; Details : Ada_Mr.Helper.String_String_Maps.Map) return String;
  
  function Create_Initialization(G_T : Group_Tag; Identifier : String; Ip : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type) return String;
  function Create_Job_Request return String;
  function Create_System_Control(G_T : Group_Tag; Message : String) return String;
  
  function Hash_To_Xml_String(Details : Ada_Mr.Helper.String_String_Maps.Map) return String;
  
  function Request_From(Node : Ada_Mr.Xml.Node_Access) return Ada_Mr.Helper.Worker_Type;
--  function Is_Master_Request(Node : Ada_Mr.Xml.Node_Access) return Boolean;
--  function Is_Mapper_Request(Node : Ada_Mr.Xml.Node_Access) return Boolean;
--  function Is_Reducer_Request(Node : Ada_Mr.Xml.Node_Access) return Boolean;
  function Is_Command(Node : Ada_Mr.Xml.Node_Access; Command : String) return Boolean;
  
  procedure Send_Error(S : GNAT.Sockets.Stream_Access; G_T : Group_Tag; Error : Ada.Exceptions.Exception_Occurrence);
  
  
  function Get_Verified_Content(Xml_Root : Ada_Mr.Xml.Node_Access) return Ada_Mr.Xml.Node_Access;
  
end Ada_Mr.Xml.Helper;