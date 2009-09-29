with Ada.Text_IO;
with Master_Count_Char; use Master_Count_Char;
with Char_Job;

procedure MR_Count_Char is
begin
    Ada.Text_IO.Put_Line("Main-Procedure is working.");

    declare
        M : Master_MR.Master_Task_Access := new Master_MR.Master_Task;
        M_C : Master_MR.Master_Console;
        J : Char_Job.Job_Vector.Vector;
    begin
--      M.Import_Jobs(J);
      M_C.Start(M);
--      M.Split_Data;
--      M.Add_Job;
--      M.Stop_Master;
      
    end;

    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("Main-Procedure is terminated.");

end MR_Count_Char;

