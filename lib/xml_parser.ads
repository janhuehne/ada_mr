with Xml;

package Xml_Parser is
  function Parse(File_Name : in String := ""; Content : in String := "") return Xml.Node_Access;
end Xml_Parser;