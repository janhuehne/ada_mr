with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Utility;
with Xml;

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
  function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
--  function From_Xml(Xml : in String) return My_Job;
  function Get_Job_Id(Job : My_Job) return Natural;
  
  -- DEPRECATED
  procedure Split_Data_Into_Jobs(Process : Add_Job_Procedure);
  
  
  procedure Split_Raw_Data;
  function Get_Next_Raw_Job return My_Job;
  
  Calculated_Jobs : Job_Vector.Vector;
  Calculated_Jobs_Cursor : Job_Vector.Cursor;
  
  Complete_String : String := "abcdefghijklmnopqrstuvwxyz-abcdefghijklmnopqrstuvwxyzzz";
  
  function Get_Next_Job_Counter(Auto_Inc : Boolean := true) return Natural;
  
  procedure Print_Job(Job : in My_Job; State : String);
  
  procedure Compute_Job(Job : in My_Job);
  
  function Job_Result_To_Xml return String;
  
  function Merge_Jobs(Xml_Node : Xml.Node_Access) return Boolean;
  
  procedure Finalize;
  
  Result_Hash : Utility.String_Integer_Maps.Map;
  
  
  Job_Counter : Natural := 1;
  
end Char_Job;
