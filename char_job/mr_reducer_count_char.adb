with Ada.Text_IO;
with Ada.Command_Line;

with Reducer_Count_Char; use Reducer_Count_Char;
with Utility;
with Xml;
with Xml_Parser;

procedure MR_Reducer_Count_Char is
begin
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  Ada.Text_IO.Put_Line("          _____               __  __ _____      _____          _                     ");
  Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  __ \        | |                    ");
  Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | |__) |___  __| |_   _  ___ ___ _ __ ");
  Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    |  _  // _ \/ _` | | | |/ __/ _ \ '__|");
  Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | | \ \  __/ (_| | |_| | (__  __/ |   ");
  Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  \_\___|\__,_|\__,_|\___\___|_|   ");
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  declare
    C   : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
    C_C : Reducer_MR.Console.Console;
  begin
    C_C.Start(C, "reducer_config.xml");
  end;
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("Map&Reduce reducer terminated.");
  Ada.Text_IO.New_Line;
end MR_Reducer_Count_Char;