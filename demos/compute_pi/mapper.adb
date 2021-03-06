with Pi_Job;
with Ada_Mr.Mapper.Main;

procedure Mapper is
  package Job renames Pi_Job;
    
  package Mapper_MR is new Ada_Mr.Mapper.Main(
    Job.Job,
    Job.From_Xml,
    Job.To_Xml,
    Job.Get_Job_Id,
    Job.Compute_Job,
    Job.Split_Result_For_Different_Reducer
  );
begin

  declare
    C   : Mapper_MR.Mapper_Task_Access := new Mapper_MR.Mapper_Task;
  begin
    C.Start(C);
  end;
  
end Mapper;
