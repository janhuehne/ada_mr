with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Utility;
with Xml;

package Char_Job is
  
  package ASU renames Ada.Strings.Unbounded;
  
  
  -- Job record
  type My_Job is record
    Job_Id            : Positive;
    Computable_String : ASU.Unbounded_String;
    Length            : Natural;
  end record;
  
  
  -- procedure which is called by the generic master
  type Add_Job_Procedure is access procedure(Job : My_Job);
  
  
  -- Vector to store all computed jobs
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
  
  
  -- Serializes a job into the xml format
  function To_Xml(Job : in My_Job) return String;
  
  
  -- Deserializes a job from the xml format
  function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  
  
  --  Returns the id from a given job
  function Get_Job_Id(Job : My_Job) return Natural;
  
  
  -- Splits the raw data into comoputable subjobs
  procedure Split_Raw_Data;
  
  
  -- Is called from the generic master to import the jobs
  function Get_Next_Raw_Job return My_Job;
  
  
  -- Used to compute the specific job ids
  function Get_Next_Job_Counter(Auto_Inc : Boolean := true) return Natural;
  
  
  -- Called by the generic system to print job in stio
  procedure Print_Job(Job : in My_Job; State : String);
  
  
  -- Called by the generic mapper to compute a job
  procedure Compute_Job(Job : in My_Job);
  
  
  -- Serializes the job result into the xml format
  function Job_Result_To_Xml return String;
  
  
  -- Procedure to merge pending job results
  procedure Merge_Jobs(Xml_Node : Xml.Node_Access);
  
  
  -- Procedure called by the reducer to handle the job result
  procedure Finalize;
  
  -- Function to split a result for serval reducers
  function Split_Result_For_Different_Reducer return Utility.String_String_Maps.Map;
  
  
  
  -- Precalculated jobs
  Calculated_Jobs : Job_Vector.Vector;
  
  
  -- Basic data
  Complete_String : String := "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.";
  
  
  -- Job result hash
  Result_Hash : Utility.String_Integer_Maps.Map;
  
  -- Counts the jobs
  Job_Counter : Natural := 1;
  
end Char_Job;
