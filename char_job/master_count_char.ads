with Master;
with Char_Job;

package Master_Count_Char is

--    package Job renames Char_Job;

    package Master_MR is new Master(Char_Job.My_Job, Char_Job.From_Xml, Char_Job.To_Xml, Char_Job.Get_Job_Id, Char_Job.Print_Job);
end Master_Count_Char;
