with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Elementary_Functions;

with Ada_Mr.Helper;
with Ada_Mr.Xml;

with Ada_Mr.Job;

package Pi_Job is
  
  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float);
  
  -- Job record definition
  type Pi_Job is new Ada_Mr.Job.Object with record
    Random_Inital_Value   : Natural;
    Pairs_Per_Map_Process : Natural := 1;
  end record;
  
  
  -- xml stuff
  overriding function To_Xml(The_Job : Pi_Job) return String;
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Pi_Job;
  
  
  -- splitting and get raw data
  procedure Split_Raw_Data;
  overriding function Get_Next_Raw_Job return Pi_Job;
  
  
  -- print job on stdio
  overriding procedure Print_Job(The_Job : Pi_Job; State : String);
  
  
  -- compute job (map function)
  overriding procedure Compute_Job(The_Job : Pi_Job);
  
  
  -- splitting the job result for serval reducers
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  
  
  -- merge job results
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean);
  
  
  procedure Finalize;
  
  
  
  
  -- package instance
  -- Vector to store all computed jobs
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => Pi_Job, 
    Index_Type => Positive
  );
  
  
  -- Precalculated jobs
  Calculated_Jobs : Job_Vector.Vector;
  
  
  -- Result storage
  In_Circle_Count : Natural := 0;
  Not_In_Circle_Count : Natural := 0;

end Pi_Job;