with Ada.Text_IO;
with Master_Count_Char; use Master_Count_Char;
with Char_Job;
with Xml;
with Xml_Parser;
with Utility;

procedure MR_Master_Count_Char is
  Config_Xml : Xml.Node_Access;
begin
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  Ada.Text_IO.Put_Line("          _____               __  __ _____      __  __           _            ");
  Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  \/  |         | |           ");
  Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | \  / | __ _ ___| |_ ___ _ __ ");
  Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    | |\/| |/ _` / __| __/ _ \ '__|");
  Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | |  | | (_| \__ \ |_  __/ |   ");
  Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  |_|\__,_|___/\__\___|_|   ");
    
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  if Utility.Does_File_Exist("master_config.xml") then
    Ada.Text_IO.Put_Line("Found config file!");
    Ada.Text_IO.Put_Line("--> Parsing config file");
    Config_Xml := Xml_Parser.Parse(File_Name => "master_config.xml");

    Ada.Text_IO.Put_Line("--> Done");
  else
    Ada.Text_IO.Put_Line("No config file found!");
  end if;
  
  
  declare
    M   : Master_MR.Master_Task_Access := new Master_MR.Master_Task;
    M_C : Master_MR.Console.Console;
  begin
    null;
    M_C.Start(M, Config_Xml);
  end;

    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("Main-Procedure is terminated.");

end MR_Master_Count_Char;

