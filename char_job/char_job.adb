with Ada.Text_IO;

package body Char_Job is
  
  function To_Xml(Job : in My_Job) return String is
    Xml_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    Ada.Strings.Unbounded.Append(Xml_String, "<?xml version=""1.0"" ?>");
    Ada.Strings.Unbounded.Append(Xml_String, "<mr-job>");
      Ada.Strings.Unbounded.Append(Xml_String, "<job-id>");
        Ada.Strings.Unbounded.Append(Xml_String, Job.Job_Id'Img);
      Ada.Strings.Unbounded.Append(Xml_String, "</job-id>");
      Ada.Strings.Unbounded.Append(Xml_String, "<computable-string>");
        Ada.Strings.Unbounded.Append(Xml_String, Job.Computable_String);
      Ada.Strings.Unbounded.Append(Xml_String, "</computable-string>");
      Ada.Strings.Unbounded.Append(Xml_String, "<responsible-reducer>");
        Ada.Strings.Unbounded.Append(Xml_String, Job.Responsible_Reducer);
      Ada.Strings.Unbounded.Append(Xml_String, "</responsible-reducer>");
    Ada.Strings.Unbounded.Append(Xml_String, "</mr-job>");
    
    return Ada.Strings.Unbounded.To_String(Xml_String);
  end To_Xml;
  
  function From_Xml(Xml : in String) return My_Job is
    J : My_Job;
  begin
    return J;
  end From_Xml;
  
  function Get_Job_Id(Job : My_Job) return Natural is
  begin
    return Job.Job_Id;
  end Get_Job_Id;
  
  procedure Split_Data_Into_Jobs(Process : Add_Job_Procedure) is
    First : Natural := Complete_String'First;
    Last  : Natural;
    Step  : Natural := 10;
  begin
    
    loop
      Last := First + Step - 1;
      
      if Last > Complete_String'Last then
        Last := Complete_String'Last;
      end if;
      
      declare
        Job : My_Job;
      begin
        Job.Job_Id := Get_Next_Job_Counter;
        Job.Computable_String := ASU.To_Unbounded_String(Complete_String(First .. Last));
        Job.Length := Last - First + 1;
        
        Process(Job);
      end;
      
      First := Last + 1;
      
      exit when Last = Complete_String'Last;
    end loop;
    
  end Split_Data_Into_Jobs;
  
  function Get_Next_Job_Counter(Auto_Inc : Boolean := true) return Natural is
    Return_Value : Natural := Job_Counter;
  begin
    if Auto_Inc = true then
      Job_Counter := Job_Counter + 1;
    end if;
      
    return Return_Value;
  end Get_Next_Job_Counter;
  
  procedure Print_Job(Job : in My_Job; Get_State : Get_State_Function) is
  begin
    Ada.Text_IO.Put(Job.Job_Id'Img);
    Ada.Text_IO.Put(ASU.To_String(Job.Computable_String));
    Ada.Text_IO.Put(Job.Responsible_Reducer);
    Ada.Text_IO.Put(Get_State(Job.Job_Id));
    Ada.Text_IO.New_Line;
  end Print_Job;
  
end Char_Job;