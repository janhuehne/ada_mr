with Ada.Text_IO;
with Ada.Command_Line;

with Utility;
with Xml;
with Xml_Parser;
with Reducer;
with Char_Job;


procedure MR_Reducer_Count_Char is
  package Reducer_MR is new Reducer(Char_Job.Merge_Jobs, Char_Job.Finalize);
begin
  
  declare
    C : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
  begin
    C.Start(C, "reducer_config.xml");
  end;
  
end MR_Reducer_Count_Char;