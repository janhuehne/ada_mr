with Ada.Text_IO;
with Master_Count_Char; use Master_Count_Char;
with Char_Job;

procedure MR_Master_Count_Char is
begin
  Ada.Text_IO.Put_Line("Main-Procedure is working.");
  
  declare
    M : Master_MR.Master_Task_Access := new Master_MR.Master_Task;
--    M_J : Master_MR.Job_Management_Task_Access := new Master_MR.Job_Management_Task;
    M_C : Master_MR.Master_Console;
    
  begin
    Ada.Text_IO.Put_Line(Master_MR.Unprocessed_Jobs.Length'Img);
    
    M_C.Start(M);
    
    Char_Job.Split_Data_Into_Jobs(Master_MR.Add_New_Job'Access);
--    M.Split_Data;
--    M.Add_Job;
--    M.Stop_Master;
      
  end;

    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("Main-Procedure is terminated.");

end MR_Master_Count_Char;

