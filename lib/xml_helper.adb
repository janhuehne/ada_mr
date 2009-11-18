with Ada.Strings.Unbounded;
with Utility;

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
    Xml_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    Ada.Strings.Unbounded.Append(Xml_String, "<?xml version=""1.0"" ?>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "<adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
    
    
    Ada.Strings.Unbounded.Append(Xml_String, "<command>");
    Ada.Strings.Unbounded.Append(Xml_String, Command);
    Ada.Strings.Unbounded.Append(Xml_String, "</command>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "<details>");
    Ada.Strings.Unbounded.Append(Xml_String, Details);
    Ada.Strings.Unbounded.Append(Xml_String, "</details>");
    
    Ada.Strings.Unbounded.Append(Xml_String, "</adamr-");
    Ada.Strings.Unbounded.Append(Xml_String, To_String(G_T));
    Ada.Strings.Unbounded.Append(Xml_String, ">");
        
    return Ada.Strings.Unbounded.To_String(Xml_String);
    
  end Xml_Command;
  
  function Xml_Command(G_T : Group_Tag; Command : String; Details : Utility.String_String_Maps.Map) return String is
    Detail_Cursor : Utility.String_String_Maps.Cursor := Utility.String_String_Maps.First(Details);
    Detail_String : Ada.Strings.Unbounded.Unbounded_String;
  begin    
    return Xml_Command(G_T, Command, Hash_To_Xml_String(Details));
  end Xml_Command;
  
  
  function Create_Initialization(G_T : Group_Tag; Identifier : String) return String is
    Details : Utility.String_String_Maps.Map;
  begin
    Details.Insert("type", "Mapper");
    Details.Insert("identifier", Identifier);
    
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
  
  function Hash_To_Xml_String(Details : Utility.String_String_Maps.Map) return String is
    Detail_Cursor : Utility.String_String_Maps.Cursor := Utility.String_String_Maps.First(Details);
    Detail_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    while Utility.String_String_Maps.Has_Element(Detail_Cursor) loop
      Ada.Strings.Unbounded.Append(Detail_String, "<");
      Ada.Strings.Unbounded.Append(Detail_String, Utility.String_String_Maps.Key(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, ">");
      Ada.Strings.Unbounded.Append(Detail_String, Utility.String_String_Maps.Element(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, "</");
      Ada.Strings.Unbounded.Append(Detail_String, Utility.String_String_Maps.Key(Detail_Cursor));
      Ada.Strings.Unbounded.Append(Detail_String, ">");
      Utility.String_String_Maps.Next(Detail_Cursor);
    end loop;
    
    return ASU.To_String(Detail_String);
  end Hash_To_Xml_String;
  
  
  function Is_Mapper_Request(Node : Xml.Node_Access) return Boolean is
  begin
    return Utility.Is_Equal(Xml.Get_Tag(Node), "adamr-mapper");
  end Is_Mapper_Request;
  
  
  function Is_Reducer_Request(Node : Xml.Node_Access) return Boolean is
  begin
    return Utility.Is_Equal(Xml.Get_Tag(Node), "adamr-reducer");
  end Is_Reducer_Request;
  
  
  function Is_Command(Node : Xml.Node_Access; Command : String) return Boolean is
  begin
    return Utility.Is_Equal(Xml.Get_Value(Node, "command"), Command);
  end Is_Command;
  
  
end Xml_Helper;