with Ada.Text_IO;
with Char_Job;
with Xml;
with Xml_Parser;
with Application_Helper;
with Master;

procedure MR_Master_Count_Char is
  package Job renames Char_Job;
  
  package Master_MR is new Master(
    Job.My_Job,
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

end MR_Master_Count_Char;