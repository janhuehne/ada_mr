with Ada.Text_IO;

package body Char_Job is
  
  function To_Xml(Job : in My_Job) return String is
    Xml_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    Ada.Strings.Unbounded.Append(Xml_String, "<?xml version=""1.0"" ?>");
    Ada.Strings.Unbounded.Append(Xml_String, "<mr-job>");
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
        Job.Computable_String := ASU.To_Unbounded_String(Complete_String(First .. Last));
        Job.Length := Last - First + 1;
        
        Process(Job);
      end;
      
      First := Last + 1;
      
      exit when Last = Complete_String'Last;
    end loop;
    
  end Split_Data_Into_Jobs;
  
end Char_Job;