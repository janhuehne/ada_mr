with Rc4_Job;
with Ada_Mr.Master.Main;

procedure Master_Rc4 is
  package Job renames Rc4_Job;
  
  package Master_MR is new Ada_Mr.Master.Main(
    Job.Rc4_Job,
    Job.From_Xml,
    Job.To_Xml,
    Job.Get_Job_Id,
    Job.Print_Job,
    Job.Split_Raw_Data,
    Job.Get_Next_Raw_Job
  );
begin
  
  declare
    M : Master_MR.Master_Task_Access := new Master_MR.Master_Task;
  begin
    M.Start(M);
  end;

end Master_Rc4;