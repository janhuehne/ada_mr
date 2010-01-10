with Ada.Exceptions;
with Ada.Text_IO;
with Simple_Xml.Xml_Io;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;

package body Ada_Mr.Xml.Parser is
  
  function Parse(File_Name : in String := ""; Content : in String := "") return Ada_Mr.Xml.Node_Access is
    Root_Node : Ada_Mr.Xml.Node_Access;
    Last_Depth : Integer := 0;
    
    procedure Open_Node(Tag : in String; Attributes : in Simple_Xml.Xml_Io.Xml_Attributes; Value : in String; Depth : in Integer) is
      Tmp_Node : Ada_Mr.Xml.Node_Access := new Ada_Mr.Xml.Node;
    begin
      Tmp_Node.Tag   := Ada.Strings.Unbounded.To_Unbounded_String(Tag);
      Tmp_Node.Value := Ada.Strings.Unbounded.To_Unbounded_String(Value); 
      
      if Depth = 1 then
        Root_Node := Tmp_Node;
      elsif Depth > 1 then
        Ada_Mr.Xml.Add_Node(Root_Node, Tmp_Node);
        Root_Node := Tmp_Node;
      end if;
    end Open_Node;
    
    procedure Close_Node (Depth : in Integer)  is
    begin
      if depth > 1 then
        Root_Node := Root_Node.Parent;
      end if;
    end Close_Node;
    
    procedure Note_Version (Version  : in String) is
    begin
      null;
    end Note_Version;
    
    procedure Note_Encoding (Encoding  : in String) is
    begin
      null;
    end Note_Encoding;
    
    procedure On_Error is
    begin
      null;
    end On_Error;
    
    procedure Parse_File is new Simple_Xml.Xml_Io.Parse(Open_Node, Close_Node, Note_Version, Note_Encoding, On_Error);
    
  begin
    if File_Name /= "" then
      Parse_File(File_Name => File_Name);
    elsif Content /= "" then
      if Is_Valid_Xml_String(Content) then
        Parse_File (Content => Content);
      else
        Ada.Exceptions.Raise_Exception(Xml_Parse_Error'Identity, "String does not start with <?xml");
      end if;
    end if;
      
    return Root_Node;
  end Parse;
  
  
  function Is_Valid_Xml_String(Str : String) return Boolean is
  begin
    return Ada_Mr.Helper.Starts_With(Str, "<?xml");
  end Is_Valid_Xml_String;

end Ada_Mr.Xml.Parser;
