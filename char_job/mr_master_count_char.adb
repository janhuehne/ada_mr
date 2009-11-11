with Ada.Text_IO;
with Master_Count_Char; use Master_Count_Char;
with Char_Job;

procedure MR_Master_Count_Char is
begin
  Ada.Text_IO.Put_Line("Main-Procedure is working.");
  
  declare
    M     : Master_MR.Master_Task_Access := new Master_MR.Master_Task;
    M_C   : Master_MR.Master_Console;
    M_O_J : Master_MR.Observe_Jobs;
  begin
    M_C.Start(M);
    M_O_J.Start;
    
    Char_Job.Split_Data_Into_Jobs(Master_MR.Add_New_Job'Access);
    
  end;

    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("Main-Procedure is terminated.");

end MR_Master_Count_Char;

