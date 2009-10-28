with Ada.Text_IO;
with Simple_Xml.Xml_Io;
with Ada.Strings.Unbounded;

package body Xml_Parser is
  
  function Parse(File_Name : in String := ""; Content : in String := "") return Xml.Node_Access is
    Root_Node : Xml.Node_Access;
    Last_Depth : Integer := 0;
    
    procedure Open_Node(Tag : in String; Attributes : in Simple_Xml.Xml_Io.Xml_Attributes; Value : in String; Depth : in Integer) is
      Tmp_Node : Xml.Node_Access := new Xml.Node;
    begin
      Tmp_Node.Tag   := Ada.Strings.Unbounded.To_Unbounded_String(Tag);
      Tmp_Node.Value := Ada.Strings.Unbounded.To_Unbounded_String(Value); 
      
      if Depth = 1 then
        Root_Node := Tmp_Node;
      elsif Depth > 1 then
        Xml.Add_Node(Root_Node, Tmp_Node);
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
      Parse_File (Content => Content);
    end if;
      
    return Root_Node;
  end Parse;
--  Xml.Print(Root_Node);
--  
--  declare
--    N : Xml.Node_Access := Xml.Find_Child_With_Tag(Root_Node, "client-id");
--  begin
--    if Xml."="(N, null) then
--      Ada.Text_IO.Put_Line("Null!");
--    else
--      Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(N.Tag));
--    end if;
--  end;
end Xml_Parser;
