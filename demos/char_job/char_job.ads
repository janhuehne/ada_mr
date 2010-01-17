with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;
with Ada_Mr.Xml;

with Ada_Mr.Job;

package Char_Job is
  
  -- Package rename
  package ASU renames Ada.Strings.Unbounded;
  
  
  -- Job record definition
  type Char_Job is new Ada_Mr.Job.Job with record
    Computable_String : ASU.Unbounded_String;
    Length            : Natural;
  end record;
  
  
  -- xml stuff
  overriding function To_Xml(The_Job : Char_Job) return String;
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Char_Job;
  
  
  -- splitting and get raw data
  procedure Split_Raw_Data;
  overriding function Get_Next_Raw_Job return Char_Job;
  
  
  -- print job on stdio
  overriding procedure Print_Job(The_Job : Char_Job; State : String);
  
  
  -- compute job (map function)
  overriding procedure Compute_Job(The_Job : Char_Job);
  
  
  -- splitting the job result for serval reducers
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  
  
  -- merge job results
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access);
  
  
  procedure Finalize;
  
  
  
  
  -- package instance
  -- Vector to store all computed jobs
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => Char_Job, 
    Index_Type => Positive
  );
  
  
  -- Precalculated jobs
  Calculated_Jobs : Job_Vector.Vector;
  
  
  -- Job result hash
  Result_Hash : Ada_Mr.Helper.String_Integer_Maps.Map;
end Char_Job;
