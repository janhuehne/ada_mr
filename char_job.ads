with Ada.Containers.Vectors;
with Master;
with Ada.Strings.Unbounded;

package Char_Job is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type My_Job is record
    Job_Id            : Positive;
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
  function Get_Job_Id(Job : My_Job) return Natural;
  
  procedure Split_Data_Into_Jobs(Process : Add_Job_Procedure);
  
  
  Complete_String : String := "abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyz";
  
  function Get_Next_Job_Counter(Auto_Inc : Boolean := true) return Natural;
  
  
  Job_Counter : Natural := 1;
  
end Char_Job;
