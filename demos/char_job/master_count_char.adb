with Char_Job;
with Ada_Mr.Master.Main;

procedure Master_Count_Char is
  package Job renames Char_Job;
  
  package Master_MR is new Ada_Mr.Master.Main(
    Job.Char_Job,
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

end Master_Count_Char;