with Ada.Text_IO;

package body Ada_Mr.Job is
  
  function Get_Job_Id(The_Job : Object) return Natural is
  begin
    return The_Job.Job_Id;
  end Get_Job_Id;
  
  function Get_Next_Job_Id return Natural is
    Return_Value : Natural := Job_Counter;
  begin
    Job_Counter := Job_Counter + 1;
      
    return Return_Value;
  end Get_Next_Job_Id;
  
  procedure Print_Job(The_Job : Object; State : String; Message : String) is
  begin
    Ada_Mr.Helper.Put(The_Job.Job_Id'Img, 10, 1);
    Ada_Mr.Helper.Put("Please override ""Print_Job"" for more details", 50, 1);
    Ada_Mr.Helper.Put(State, 20);
    Ada_Mr.Helper.Put(Message, 20);
    Ada.Text_IO.New_Line;
  end;
  
end Ada_Mr.Job;