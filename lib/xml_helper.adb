with Ada.Strings.Unbounded;
with Application_Helper;
with Crypto_Helper;
with Logger;
with Ada.Strings.Fixed;

package body Xml_Helper is

  function To_String(G_T : Group_Tag) return String is
  begin
    case G_T is
      when Mapper => return "mapper";
      when Reducer => return "reducer";
      when Master => return "master";
    end case;
  end To_String;
  
  function Xml_Command(G_T : Group_Tag; Command : String; Details : String := "") return String is
  begin
    return Xml_Command(G_T, Command, "", Details);
  end Xml_Command;
  
  
  function Xml_Command(G_T : Group_Tag; Command : String; Access_Token : String; Details : String := "") return String is
    Xml_String  : Ada.Strings.Unbounded.Unbounded_String;
    Xml_Content : Ada.Strings.Unbounded.Unbounded_String;
  begin
    
    -- <-- Content area
    if Access_Token /= "" then
      Ada.Strings.Unbounded.Append(Xml_Content, "<access_token>");
      Ada.Strings.Unbounded.Append(Xml_Content, Access_Token);
      Ada.Strings.Unbounded.Append(Xml_Content, "</access_token>");
      
    end if;
    
    Ada.Strings.Unbounded.Append(Xml_Content, "<command>");
    Ada.Strings.Unbounded.Append(Xml_Content, Command);
    Ada.Strings.Unbounded.Append(Xml_Content, "</command>");
    
    Ada.Strings.Unbounded.Append(Xml_Content, "<details>");
    Ada.Strings.Unbounded.Append(Xml_Content, Details);
    Ada.Strings.Unbounded.Append(Xml_Content, "</details>");
    -- Content area -->
    
    
    Ada.Strings.Unbounded.Append(Xml_String, "<?xml version=""1.0"" ?>");
    Ada.Strings.Unbounded.Append(Xml_String, "<adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
    
    Ada.Strings.Unbounded.Append(Xml_String, "<hmac>");
    Ada.Strings.Unbounded.Append(Xml_String, Crypto_Helper.Compute_HMAC(ASU.To_String(Xml_Content),"MastMappReducer"));
    Ada.Strings.Unbounded.Append(Xml_String, "</hmac>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "<content>");
    Ada.Strings.Unbounded.Append(Xml_String, Xml_Content);
    Ada.Strings.Unbounded.Append(Xml_String, "</content>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "</adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
    
    return Ada.Strings.Unbounded.To_String(Xml_String);
  end Xml_Command;
  
  function Xml_Command(G_T : Group_Tag; Command : String; Details : Application_Helper.String_String_Maps.Map) return String is
  begin
    return Xml_Command(G_T, Command, "", Details);
  end Xml_Command;
  
  function Xml_Command(G_T : Group_Tag; Command : String; Access_Token : String; Details : Application_Helper.String_String_Maps.Map) return String is
    Detail_Cursor : Application_Helper.String_String_Maps.Cursor := Application_Helper.String_String_Maps.First(Details);
    Detail_String : Ada.Strings.Unbounded.Unbounded_String;
  begin    
    return Xml_Command(
      G_T     =>  G_T, 
      Command => Command,
      Access_Token => Access_Token,
      Details => Hash_To_Xml_String(Details)
    );
  end Xml_Command;
  
  
  function Create_Initialization(G_T : Group_Tag; Identifier : String; Ip : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type) return String is
    use Ada.Strings;
    use Ada.Strings.Fixed;
    
    Details : Application_Helper.String_String_Maps.Map;
  begin
    Details.Insert("type", Trim(Xml_Helper.To_String(G_T), Both));
    Details.Insert("identifier", Trim(Identifier, Both));
    Details.Insert("ip", Trim(GNAT.Sockets.Image(Ip), Both));
    Details.Insert("port", Trim(Port'Img, Both));
    
    return Xml_Command(G_T, "initialization", Details);
  end Create_Initialization;
  
  function Create_Job_Request return String is
  begin
    return Xml_Command(Mapper, "job_request");
  end Create_Job_Request;
  
  function Create_System_Control(G_T : Group_Tag; Message : String) return String is
    Xml_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    Ada.Strings.Unbounded.Append(Xml_String, "<?xml version=""1.0"" ?>");
    Ada.Strings.Unbounded.Append(Xml_String, "<adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
    
    Ada.Strings.Unbounded.Append(Xml_String, "<sysctrl><message>");
    Ada.Strings.Unbounded.Append(Xml_String, Message);
    Ada.Strings.Unbounded.Append(Xml_String, "</sysctrl></message>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "</adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
    
    return Ada.Strings.Unbounded.To_String(Xml_String);
  end Create_System_Control;
  
  function Hash_To_Xml_String(Details : Application_Helper.String_String_Maps.Map) return String is
    Detail_Cursor : Application_Helper.String_String_Maps.Cursor := Application_Helper.String_String_Maps.First(Details);
    Detail_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    while Application_Helper.String_String_Maps.Has_Element(Detail_Cursor) loop
      Ada.Strings.Unbounded.Append(Detail_String, "<");
      Ada.Strings.Unbounded.Append(Detail_String, Application_Helper.String_String_Maps.Key(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, ">");
      Ada.Strings.Unbounded.Append(Detail_String, Application_Helper.String_String_Maps.Element(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, "</");
      Ada.Strings.Unbounded.Append(Detail_String, Application_Helper.String_String_Maps.Key(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, ">");
      Application_Helper.String_String_Maps.Next(Detail_Cursor);
    end loop;
    
    return ASU.To_String(Detail_String);
  end Hash_To_Xml_String;
  
  function Request_From(Node : Xml.Node_Access) return Application_Helper.Worker_Type is
    Node_Tag : String := Xml.Get_Tag(Node);
  begin
    if Application_Helper.Is_Equal(Node_Tag, "adamr-master") then
      return Application_Helper.Master;
    elsif Application_Helper.Is_Equal(Node_Tag, "adamr-mapper") then
      return Application_Helper.Mapper;
    elsif Application_Helper.Is_Equal(Node_Tag, "adamr-reducer") then
      return Application_Helper.Reducer;
    end if;
    
    return Application_Helper.Invalid;
  end;
  
--  function Is_Master_Request(Node : Xml.Node_Access) return Boolean is
--  begin
--    return Application_Helper.Is_Equal(Xml.Get_Tag(Node), "adamr-master");
--  end Is_Master_Request;
--  
--  
--  function Is_Mapper_Request(Node : Xml.Node_Access) return Boolean is
--  begin
--    return Application_Helper.Is_Equal(Xml.Get_Tag(Node), "adamr-mapper");
--  end Is_Mapper_Request;
--  
--  
--  function Is_Reducer_Request(Node : Xml.Node_Access) return Boolean is
--  begin
--    return Application_Helper.Is_Equal(Xml.Get_Tag(Node), "adamr-reducer");
--  end Is_Reducer_Request;
  
  
  function Is_Command(Node : Xml.Node_Access; Command : String) return Boolean is
  begin
    return Application_Helper.Is_Equal(Xml.Get_Value(Node, "command"), Command);
  end Is_Command;
  
  
  procedure Send_Error(S : GNAT.Sockets.Stream_Access; G_T : Group_Tag; Error : Ada.Exceptions.Exception_Occurrence) is
  begin
    String'Output(S, Xml_Helper.Xml_Command(
      G_T     => G_T,
      Command => "error",
      Details => "<message>" & Ada.Exceptions.Exception_Name(Error) & "(" & Ada.Exceptions.Exception_Message(Error) & ")</message>"
    ));
  end;
  
  
  function Get_Verified_Content(Xml_Root : Xml.Node_Access) return Xml.Node_Access is
    Xml_Content : Xml.Node_Access := Xml.Find_Child_With_Tag(Xml_Root, "content");
  begin
    
    Logger.Put_Line(Xml.Node_Content_To_String(Xml_Content), Logger.Info);
    
    
    if Crypto_Helper.Compute_HMAC(Xml.Node_Content_To_String(Xml_Content), "MastMappReducer") /= Xml.Get_Value(Xml_Root, "hmac") then
      raise Crypto_Helper.Wrong_HMAC;
    end if;
      
    Logger.Put_Line("HMAC verified", Logger.Info);
    
    return Xml_Content;
  end Get_Verified_Content;
  
end Xml_Helper;