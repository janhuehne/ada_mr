with Rc4_Job;
with Ada_Mr.Reducer.Main;


procedure Reducer_Rc4 is
  package Job renames Rc4_Job;
  
  package Reducer_MR is new Ada_Mr.Reducer.Main(
    Job.Merge_Job_Results, 
    Job.Finalize
  );
  
begin
  
  declare
    C : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
  begin
    C.Start(C);
  end;
  
end Reducer_Rc4;