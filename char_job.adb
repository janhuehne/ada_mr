with Ada.Strings.Unbounded;

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
  
  function Split_Job(Job : in Main_Job) return Job_Vector.Vector is
    Jobs : Job_Vector.Vector;
  begin
    return Jobs;
  end Split_Job;
  
end Char_Job;