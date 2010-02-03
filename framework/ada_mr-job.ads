with Ada_Mr.Xml;
with Ada_Mr.Helper;

package Ada_Mr.Job is
  
  -- record
  type Object is abstract tagged record
    Job_Id : Natural;
  end record;
  
  -- Serializes a job into the xml format
  function To_Xml(The_Job : Object) return String is abstract;
  
  -- Deserializes a job from the xml format
  function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Object is abstract;
  
  --  Returns the id from a given job
  function Get_Job_Id(The_Job : Object) return Natural;
  
  -- Splits the raw data into comoputable subjobs
  procedure Split_Raw_Data is abstract;
  
  -- Is called from the generic master to import the jobs
  function Get_Next_Raw_Job return Object is abstract;
  
  -- Called by the generic system to print job in stio
  procedure Print_Job(The_Job : Object; State : String; Message : String);
  
  -- Called by the generic mapper to compute a job
  procedure Compute_Job(The_Job : Object) is abstract;
  
  -- Function to split a result for serval reducers
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is abstract;
  
  -- Procedure to merge pending job results
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean) is abstract;
  
  -- Procedure called by the reducer to handle the job result
  procedure Finalize is abstract;
  
  
  function Get_Next_Job_Id return Natural;

private
  
  -- Counts the jobs
  Job_Counter : Natural := 1;


end Ada_Mr.Job;