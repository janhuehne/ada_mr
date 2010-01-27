with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Ada_Mr.Helper;
with Ada_Mr.Xml;
with Ada_Mr.Job;

with Rc_4;

package Rc4_Job is
  -- Package rename
  package ASU renames Ada.Strings.Unbounded;
     
  -- Job record definition
  type Rc4_Job is new Ada_Mr.Job.Object with record
    Plain_Text          : Rc_4.Unsigned_Byte_Array(1 .. 10);
    Cipher_Text         : Rc_4.Unsigned_Byte_Array(1 .. 10);
    Start_Most_Sig_Byte : Rc_4.Unsigned_Byte;
    Most_Sig_Byte_Range : Natural;
  end record;
  
  
  -- xml stuff
  overriding function To_Xml(The_Job : Rc4_Job) return String;
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Rc4_Job;
  
  
  -- splitting and get raw data
  procedure Split_Raw_Data;
  overriding function Get_Next_Raw_Job return Rc4_Job;
  
  
  -- print job on stdio
  overriding procedure Print_Job(The_Job : Rc4_Job; State : String);
  
  
  -- compute job (map function)
  overriding procedure Compute_Job(The_Job : Rc4_Job);
  
  
  -- splitting the job result for serval reducers
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  
  
  -- merge job results
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean);
  
  
  procedure Finalize;
  
  
  
  
  -- package instance
  -- Vector to store all computed jobs
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => Rc4_Job, 
    Index_Type => Positive
  );
  
  
  -- Precalculated jobs
  Calculated_Jobs : Job_Vector.Vector;
  
  Key_Found : Boolean := False;
  Found_Key : Rc_4.Key_Type;
private

  function Byte_Array_To_Xml(Byte_Array : Rc_4.Unsigned_Byte_Array) return String ;
  procedure Byte_Array_From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access; Byte_Array : out Rc_4.Unsigned_Byte_Array);
end Rc4_Job;
