with Ada_Mr.Xml;

package Ada_Mr.Xml.Parser is
  Xml_Parse_Error : Exception;
  
  function Parse(File_Name : in String := ""; Content : in String := "") return Ada_Mr.Xml.Node_Access;
  function Is_Valid_Xml_String(Str : String) return Boolean;
end Ada_Mr.Xml.Parser;