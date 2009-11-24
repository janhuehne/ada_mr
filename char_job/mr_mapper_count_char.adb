with Ada.Text_IO;
with Ada.Command_Line;

with Mapper_Count_Char; use Mapper_Count_Char;
with Utility;
with Xml;
with Xml_Parser;

procedure MR_Mapper_Count_Char is
  Config_Xml : Xml.Node_Access;
begin
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  Ada.Text_IO.Put_Line("          _____               __  __ _____      __  __                             ");
  Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  \/  |                            ");
  Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | \  / | __ _ _ __  _ __   ___ _ __ ");
  Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    | |\/| |/ _` | '_ \| '_ \ / _ \ '__|");
  Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | |  | | (_| | |_) | |_) |  __/ |   ");
  Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  |_|\__,_| .__/| .__/ \___|_|   ");
  Ada.Text_IO.Put_Line("                                                            | |   | |              ");
  Ada.Text_IO.Put_Line("                                                            |_|   |_|               ");
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  if Utility.Does_File_Exist("mapper_config.xml") then
    Ada.Text_IO.Put_Line("Found config file!");
    Ada.Text_IO.Put_Line("--> Parsing config file");
    Config_Xml := Xml_Parser.Parse(File_Name => "mapper_config.xml");
    
    Ada.Text_IO.Put_Line("--> Done");
  else
    Ada.Text_IO.Put_Line("No config file found!");
  end if;
    
--  Ada.Text_IO.Put_Line(Ada.Command_Line.Argument_Count'Img);

  declare
    C   : Mapper_MR.Mapper_Task_Access := new Mapper_MR.Mapper_Task;
    C_C : Mapper_MR.Console.Console;
  begin
    C_C.Start(C, Config_Xml);
  end;
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("MR Mapper terminated.");
  Ada.Text_IO.New_Line;
end MR_Mapper_Count_Char;