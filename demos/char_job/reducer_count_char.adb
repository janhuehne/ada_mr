with Char_Job;
with Ada_Mr.Reducer.Main;


procedure Reducer_Count_Char is
  package Job renames Char_Job;
  
  package Reducer_MR is new Ada_Mr.Reducer.Main(
    Job.Merge_Job_Results, 
    Char_Job.Finalize
  );
  
begin
  
  declare
    C : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
  begin
    C.Start(C);
  end;
  
end Reducer_Count_Char;