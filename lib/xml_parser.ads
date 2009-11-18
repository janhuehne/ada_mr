with Xml;

package Xml_Parser is
  Xml_Parse_Error : Exception;
  
  function Parse(File_Name : in String := ""; Content : in String := "") return Xml.Node_Access;
  function Is_Valid_Xml_String(Str : String) return Boolean;
end Xml_Parser;