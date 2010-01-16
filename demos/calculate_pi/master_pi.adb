with Pi_Job;
with Ada_Mr.Master.Main;

procedure Master_Pi is
  package Job renames Pi_Job;
  
  package Master_MR is new Ada_Mr.Master.Main(
    Job.Pi_Job,
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
    M.Start(M, "master_config.xml");
  end;

end Master_Pi;