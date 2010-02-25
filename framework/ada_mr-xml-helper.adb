with Ada.Strings.Unbounded;
with Ada_Mr.Helper;
with Ada_Mr.Crypt.Helper;
with Ada_Mr.Logger;
with Ada.Strings.Fixed;

package body Ada_Mr.Xml.Helper is
  
  ---------------
  -- To_String --
  ---------------
  function To_String
    (G_T : Group_Tag)
    return String 
  is
  begin
    case G_T is
      when Mapper => return "mapper";
      when Reducer => return "reducer";
      when Master => return "master";
    end case;
  end To_String;
  
  
  
  -----------------
  -- Xml_Command --
  -----------------
  function Xml_Command
    (G_T     : Group_Tag; 
     Command : String; 
     Details : String := "")
    return String 
  is
  begin
    return Xml_Command(G_T, Command, "", Details);
  end Xml_Command;
  
  
  function Xml_Command
    (G_T          : Group_Tag; 
     Command      : String; 
     Access_Token : String; 
     Details      : String := "")
    return String 
  is
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
    Ada.Strings.Unbounded.Append(Xml_String, Ada_Mr.Crypt.Helper.Compute_HMAC(ASU.To_String(Xml_Content), Ada_Mr.Helper.Read_Configuration("CRYPTO", "HMAC")));
    Ada.Strings.Unbounded.Append(Xml_String, "</hmac>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "<content>");
    Ada.Strings.Unbounded.Append(Xml_String, Xml_Content);
    Ada.Strings.Unbounded.Append(Xml_String, "</content>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "</adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
    
    return Ada.Strings.Unbounded.To_String(Xml_String);
  end Xml_Command;
  
  
  function Xml_Command
    (G_T     : Group_Tag;
     Command : String;
     Details : Ada_Mr.Helper.String_String_Maps.Map)
    return String 
  is
  begin
    return Xml_Command(G_T, Command, "", Details);
  end Xml_Command;
  
  
  function Xml_Command
    (G_T          : Group_Tag;
     Command      : String;
     Access_Token : String;
     Details      : Ada_Mr.Helper.String_String_Maps.Map)
    return String 
  is
    Detail_Cursor : Ada_Mr.Helper.String_String_Maps.Cursor := Ada_Mr.Helper.String_String_Maps.First(Details);
    Detail_String : Ada.Strings.Unbounded.Unbounded_String;
  begin    
    return Xml_Command(
      G_T          =>  G_T, 
      Command      => Command,
      Access_Token => Access_Token,
      Details      => Hash_To_Xml_String(Details)
    );
  end Xml_Command;
  
  
  
  ---------------------------
  -- Create_Initialization --
  ---------------------------
  function Create_Initialization
    (G_T        : Group_Tag;
     Identifier : String;
     Ip         : GNAT.Sockets.Inet_Addr_Type; 
     Port       : GNAT.Sockets.Port_Type)
    return String 
  is
    use Ada.Strings;
    use Ada.Strings.Fixed;
    
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("type", Trim(Ada_Mr.Xml.Helper.To_String(G_T), Both));
    Details.Insert("identifier", Trim(Identifier, Both));
    Details.Insert("ip", Trim(GNAT.Sockets.Image(Ip), Both));
    Details.Insert("port", Trim(Port'Img, Both));
    
    return Xml_Command(G_T, "initialization", Details);
  end Create_Initialization;
  
  
  
  ------------------------
  -- Create_Job_Request --
  ------------------------
  function Create_Job_Request
    return String 
  is
  begin
    return Xml_Command(Mapper, "job_request");
  end Create_Job_Request;
  
  
  
  ---------------------------
  -- Create_System_Control --
  ---------------------------
  function Create_System_Control
    (G_T     : Group_Tag;
     Message : String)
    return String 
  is
  begin
    return Xml_Command(G_T, "sysctrl", "", "<message>" & Message & "</message>");
  end Create_System_Control;
  
  
  
  ------------------------
  -- Hash_To_Xml_String --
  ------------------------
  function Hash_To_Xml_String
    (Details : Ada_Mr.Helper.String_String_Maps.Map)
    return String 
  is
    Detail_Cursor : Ada_Mr.Helper.String_String_Maps.Cursor := Ada_Mr.Helper.String_String_Maps.First(Details);
    Detail_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    while Ada_Mr.Helper.String_String_Maps.Has_Element(Detail_Cursor) loop
      Ada.Strings.Unbounded.Append(Detail_String, "<");
      Ada.Strings.Unbounded.Append(Detail_String, Ada_Mr.Helper.String_String_Maps.Key(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, ">");
      Ada.Strings.Unbounded.Append(Detail_String, Ada_Mr.Helper.String_String_Maps.Element(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, "</");
      Ada.Strings.Unbounded.Append(Detail_String, Ada_Mr.Helper.String_String_Maps.Key(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, ">");
      Ada_Mr.Helper.String_String_Maps.Next(Detail_Cursor);
    end loop;
    
    return ASU.To_String(Detail_String);
  end Hash_To_Xml_String;
  
  
  
  ------------------
  -- Request_From --
  ------------------
  function Request_From
    (Node : Ada_Mr.Xml.Node_Access)
    return Ada_Mr.Helper.Worker_Type
  is
    Node_Tag : String := Ada_Mr.Xml.Get_Tag(Node);
  begin
    if Ada_Mr.Helper.Is_Equal(Node_Tag, "adamr-master") then
      return Ada_Mr.Helper.Master;
    elsif Ada_Mr.Helper.Is_Equal(Node_Tag, "adamr-mapper") then
      return Ada_Mr.Helper.Mapper;
    elsif Ada_Mr.Helper.Is_Equal(Node_Tag, "adamr-reducer") then
      return Ada_Mr.Helper.Reducer;
    end if;
    
    return Ada_Mr.Helper.Invalid;
  end;
  
  
  
  ----------------
  -- Is_Command --
  ----------------
  function Is_Command
    (Node    : Ada_Mr.Xml.Node_Access;
     Command : String)
    return Boolean 
  is
  begin
    return Ada_Mr.Helper.Is_Equal(Ada_Mr.Xml.Get_Value(Node, "command"), Command);
  end Is_Command;
  
  
  
  ----------------
  -- Send_Error --
  ----------------
  procedure Send_Error
    (S     : GNAT.Sockets.Stream_Access; 
     G_T   : Group_Tag; 
     Error : Ada.Exceptions.Exception_Occurrence) 
  is
  begin
    String'Output(S, Ada_Mr.Xml.Helper.Xml_Command(
      G_T     => G_T,
      Command => "error",
      Details => "<message>" & Ada.Exceptions.Exception_Name(Error) & "(" & Ada.Exceptions.Exception_Message(Error) & ")</message>"
    ));
  end;
  
  
  
  --------------------------
  -- Get_Verified_Content --
  --------------------------
  function Get_Verified_Content
    (Xml_Root : Ada_Mr.Xml.Node_Access)
    return Ada_Mr.Xml.Node_Access
  is
    Xml_Content : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Find_Child_With_Tag(Xml_Root, "content");
  begin
    Ada_Mr.Logger.Put_Line(Ada_Mr.Xml.Node_Content_To_String(Xml_Content), Ada_Mr.Logger.Info);
    
    if Ada_Mr.Crypt.Helper.Compute_HMAC(
        Ada_Mr.Xml.Node_Content_To_String(Xml_Content), 
        Ada_Mr.Helper.Read_Configuration("CRYPTO", "HMAC")
      ) /= Ada_Mr.Xml.Get_Value(Xml_Root, "hmac") then
      Ada.Exceptions.Raise_Exception(Ada_Mr.Crypt.Helper.Wrong_HMAC'Identity, "Content: " 
        & Ada_Mr.Xml.Node_Content_To_String(Xml_Content));
    end if;
      
    Ada_Mr.Logger.Put_Line("HMAC verified", Ada_Mr.Logger.Info);
    
    return Xml_Content;
  end Get_Verified_Content;
  
end Ada_Mr.Xml.Helper;