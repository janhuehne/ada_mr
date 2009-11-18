with Master;
with Char_Job;

package Master_Count_Char is
  
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
  
end Master_Count_Char;