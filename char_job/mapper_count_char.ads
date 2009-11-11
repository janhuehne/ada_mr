with Mapper;
with Char_Job;

package Mapper_Count_Char is
    
    package Job renames Char_Job;
    
    package Mapper_MR is new Mapper(
      Job.My_Job, 
      Job.From_Xml, 
      Job.To_Xml, 
      Job.Get_Job_Id, 
      Job.Print_Job, 
      Job.Compute_Job, 
      Job.Job_Result_To_Xml
    );
    
end Mapper_Count_Char;
