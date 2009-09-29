with Ada.Text_IO;
with Xml;
with Xml_Parser;

procedure Test is
  S : String := "<?xml version=""1.0"" ?><adamr-client><type>Mapper</type><host>localhost</host></adamr-client>";
--  Xml_Root : Xml.Node_Access := Xml_Parser.Parse(File_Name => "/Users/jahu/DATAcenter/BUW/Diplomarbeit/Tests/pool_server/client.xml");
  Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => S);
  
  
begin
  Ada.Text_IO.Put_Line(S);
  Xml.Print(Xml_Root);
end Test;