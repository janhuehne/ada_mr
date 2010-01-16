with Ada.Command_Line;
with Ada_Mr.Reducer.Main;
with Pi_Job;

procedure Reducer_Pi is
  package Job renames Pi_Job;
    
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
  
end Reducer_Pi;