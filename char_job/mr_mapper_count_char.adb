with Ada.Text_IO;
with Ada.Command_Line;
with Char_Job;
with Application_Helper;
with Xml;
with Xml_Parser;
with Mapper;

procedure MR_Mapper_Count_Char is
   package Job renames Char_Job;
    
    package Mapper_MR is new Mapper(
      Job.My_Job,
      Job.From_Xml,
      Job.To_Xml,
      Job.Get_Job_Id,
      Job.Print_Job,
      Job.Compute_Job,
      Job.Split_Result_For_Different_Reducer
    );
begin

  declare
    C   : Mapper_MR.Mapper_Task_Access := new Mapper_MR.Mapper_Task;
  begin
    C.Start(C, "mapper_config.xml");
  end;
  
end MR_Mapper_Count_Char;