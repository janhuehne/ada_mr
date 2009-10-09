with Ada.Containers.Vectors;
with Master;
with Ada.Strings.Unbounded;

package Char_Job is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type My_Job is record
    Computable_String : ASU.Unbounded_String;
    Responsible_Reducer : String(1..15) := "127.000.000.001";
    Length : Natural;
  end record;
    
  type Add_Job_Procedure is access procedure(Job : My_Job);
  
    
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
  
  function To_Xml(Job : in My_Job) return String;
  function From_Xml(Xml : in String) return My_Job;
  
  procedure Split_Data_Into_Jobs(Process : Add_Job_Procedure);
  
  
  Complete_String : String := "abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz";
  
end Char_Job;
