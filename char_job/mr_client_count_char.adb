with Ada.Text_IO;
with Ada.Command_Line;

with Client_Count_Char; use Client_Count_Char;
--with Char_Job;

procedure MR_Client_Count_Char is
begin
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  Ada.Text_IO.Put_Line(" _____ ____  _____     _____ _____    _____ _ _         _   ");
  Ada.Text_IO.Put_Line("|  _  |    \|  _  |___|     | __  |  |     | |_|___ ___| |_ ");
  Ada.Text_IO.Put_Line("|     |  |  |     |___| | | |    -|  |   --| | | -_|   |  _|");
  Ada.Text_IO.Put_Line("|__|__|____/|__|__|   |_|_|_|__|__|  |_____|_|_|___|_|_|_|  ");
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  Ada.Text_IO.New_Line;
  
  if Ada.Command_Line.Argument_Count = 0 then
    Ada.Text_IO.Put_Line("ERROR: Configuration file missing!");
  else
    declare
      C   : Client_MR.Client_Task_Access := new Client_MR.Client_Task;
      C_C : Client_MR.Console;
      
    begin
      C_C.Start(C);
    end;
  end if;
  
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put_Line("Map&Reduce client terminated.");
  Ada.Text_IO.New_Line;
end MR_Client_Count_Char;