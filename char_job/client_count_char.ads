with Client;
with Char_Job;

package Client_Count_Char is

--    package Job renames Char_Job;

    package Client_MR is new Client(Char_Job.My_Job, Char_Job.From_Xml, Char_Job.To_Xml, Char_Job.Get_Job_Id, Char_Job.Print_Job);
end Client_Count_Char;
