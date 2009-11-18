with Ada.Containers.Vectors;
with Echo;
with Xml;
with Master_Helper;
with Server;
with Ada.Strings.Unbounded;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String);
  with procedure Split_Raw_Data;
  with function Get_Next_Raw_Job return My_Job;
  
package Master is
  
  package ASU renames Ada.Strings.Unbounded;
  
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
  
  
  type Master_Task;
  type Master_Task_Access is access Master_Task;
  
  
  task type Master_Task is
    entry Start_Master(M : Master_Task_Access);
    entry Stop_Master;
  end Master_Task;
  
  
  task type Master_Console is
    entry Start(M_Arg : Master_Task_Access);
  end Master_Console;
  
  
  task type Observe_Jobs is
    entry Start;
  end Observe_Jobs;
  

--  function Get_Next_Job(Remove_From_Vector : Boolean := true) return My_Job;
  
  procedure Print_Jobs;
  
  type My_Job_Access is access My_Job;
  
  -- #### New Stuff. Server rewrite!
  type Job_Entry_Record is record
    Job   : My_Job;
    State : Master_Helper.Job_State := Master_Helper.Pending;
  end record;
  
  type Job_Entry_Record_Access is access Job_Entry_Record;
  
  function "="(Left, Right : Job_Entry_Record_Access) return Boolean;
  
  package Job_Entry_Record_Vectors is new Ada.Containers.Vectors(
    Element_Type => Job_Entry_Record_Access,
    Index_Type => Positive,
    "=" => "="
  );
  
  
  procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State);
  
  function Job_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
  
  protected Jobs is
    procedure Add(Job : My_Job);
    function Get_By_Id(Id : Natural) return Job_Entry_Record_Access;
    function Get_Next_Pending return Job_Entry_Record_Access;
    function Count return Natural;
    function Count_By_State(State : Master_Helper.Job_State) return Natural;
    procedure Print;
  private
    Jobs : Job_Entry_Record_Vectors.Vector;
  end Jobs;
  
  
  protected Worker is
    procedure Add(New_Worker : Master_Helper.Worker_Record_Access);
    procedure Print;
  private
    Worker : Master_Helper.Worker_Entry_Vectors.Vector;
  end Worker;
  
  
--  procedure Change_Job_State(Job : Job_Entry_Record_Access; State : Master_Helper.Job_State) is

  
  package Master_Server is new Server(
    My_Job,
    Job_Entry_Record_Access,
    Worker.Add,
    Jobs.Get_By_Id,
    Jobs.Get_Next_Pending,
    Change_Job_State,
    Job_To_Xml
  );
  
  
end Master;