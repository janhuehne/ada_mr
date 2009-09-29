with Ada.Containers.Vectors;
with Master;

package Char_Job is
  
  type My_Job is record
    Computable_String : String(1..20) := "aaaaaabbbbbbbbcccccc";
    Responsible_Reducer : String(1..15) := "127.000.000.001";
    Length : Integer;
  end record;
  
  type Main_Job is record
    Complete_String : String(1..40) := "aaaaaabbbbbbbbccccccaaaaaabbbbbbbbcccccc";
  end record;
    
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
    
  function To_Xml(Job : in My_Job) return String;
  function From_Xml(Xml : in String) return My_Job;
  function Split_Job(Job : in Main_Job) return Job_Vector.Vector;
  
end Char_Job;
