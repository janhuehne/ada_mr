with Ada.Text_IO;
with Ada.Command_Line;

with Ada_Mr.Helper;
with Ada_Mr.Xml;
with Ada_Mr.Xml.Parser;

with Char_Job;


with Ada_Mr.Reducer.Main;


procedure MR_Reducer_Count_Char is
  package Reducer_MR is new Ada_Mr.Reducer.Main(Char_Job.Merge_Jobs, Char_Job.Finalize);
begin
  
  declare
    C : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
  begin
    C.Start(C, "reducer_config.xml");
  end;
  
end MR_Reducer_Count_Char;