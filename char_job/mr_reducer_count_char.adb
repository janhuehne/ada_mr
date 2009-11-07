with Ada.Text_IO;
with Ada.Command_Line;

with Reducer_Count_Char; use Reducer_Count_Char;
with Utility;

procedure MR_Reducer_Count_Char is
begin
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
--  Ada.Text_IO.Put_Line(" _____ ____  _____     _____ _____    _____ _ _         _   ");
--  Ada.Text_IO.Put_Line("|  _  |    \|  _  |___|     | __  |  |     | |_|___ ___| |_ ");
--  Ada.Text_IO.Put_Line("|     |  |  |     |___| | | |    -|  |   --| | | -_|   |  _|");
--  Ada.Text_IO.Put_Line("|__|__|____/|__|__|   |_|_|_|__|__|  |_____|_|_|___|_|_|_|  ");
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  
  if Utility.Does_File_Exist("reducer_config.xml") then
    Ada.Text_IO.Put_Line("Found config file!");
  else
    Ada.Text_IO.Put_Line("No config file found!");
  end if;
    
--  Ada.Text_IO.Put_Line(Ada.Command_Line.Argument_Count'Img);

  declare
    C   : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
    C_C : Reducer_MR.Console;
  begin
    C_C.Start(C);
  end;
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("Map&Reduce reducer terminated.");
  Ada.Text_IO.New_Line;
end MR_Reducer_Count_Char;