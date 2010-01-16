package body Ada_Mr.Job is
  
  function Get_Job_Id(The_Job : Job) return Natural is
  begin
    return The_Job.Job_Id;
  end Get_Job_Id;
  
  function Get_Next_Job_Id return Natural is
    Return_Value : Natural := Job_Counter;
  begin
    Job_Counter := Job_Counter + 1;
      
    return Return_Value;
  end Get_Next_Job_Id;
  
end Ada_Mr.Job;