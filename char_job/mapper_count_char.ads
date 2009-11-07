with Mapper;
with Char_Job;

package Mapper_Count_Char is

--    package Job renames Char_Job;

    package Mapper_MR is new Mapper(Char_Job.My_Job, Char_Job.From_Xml, Char_Job.To_Xml, Char_Job.Get_Job_Id, Char_Job.Print_Job, Char_Job.Compute_Job, Char_Job.Job_Result_To_Xml);
end Mapper_Count_Char;
