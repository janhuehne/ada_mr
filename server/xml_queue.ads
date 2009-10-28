with Worker;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Xml_Queue is
  
  Job_Not_Found : Exception;
  Invalid_Job_State : Exception;
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Job_State is (Pending, In_Progress, Done);
  
  type Xml_Job_Entry is record
    Id    : Positive;
    Xml   : ASU.Unbounded_String;
    State : Job_State := Pending;
  end record;
  
  type Xml_Job_Entry_Access is access Xml_Job_Entry;
  
  procedure Add_Job(Job_Id : Natural; Xml_Representation : String; State : Job_State := Pending);
  
  function Count_Jobs(State : Job_State) return Natural;
  
  function Find_Job_By_Id(Job_Id : Natural) return Xml_Job_Entry_Access;
  
  function Find_First_Job_By_State(State : Job_State) return Xml_Job_Entry_Access;
  
  procedure Change_Job_State(Job_Id : Natural; New_State : Job_State);
  
  procedure Change_Job_State(Job : Xml_Job_Entry_Access; New_State : Job_State);
    
  procedure Change_Job_State(Job_Id : String; New_State : String);
  
  function Get_Job_State(Job_Id : Integer) return String;
  
  procedure Print_Jobs;
  
  function "="(Left, Right : Xml_Job_Entry_Access) return Boolean;
  
  function To_String(State : Job_State) return String;
  
  package Unbounded_String_Vector is new Ada.Containers.Vectors(
    Element_Type => Xml_Job_Entry_Access,
    Index_Type => Positive
  );
  
  Jobs : Unbounded_String_Vector.Vector;
  
end Xml_Queue;