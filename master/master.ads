with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Xml;
with Master_Helper;
with Master_Server;
with Generic_Console;
with Generic_Observer;

with Utility;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String);
  with procedure Split_Raw_Data;
  with function Get_Next_Raw_Job return My_Job;
  
package Master is
  
----------------------------------------------------
-- PACKAGE RENAMES                                 -
----------------------------------------------------
  package ASU renames Ada.Strings.Unbounded;



----------------------------------------------------
-- PACKAGE INSTANCES                               -
----------------------------------------------------
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
  
  
  
----------------------------------------------------
-- MASTER TASK                                     -
----------------------------------------------------
  type Master_Task;
  type Master_Task_Access is access Master_Task;
  
  task type Master_Task is
    entry Start(Self : Master_Task_Access; Config_File : String);
    entry Stop;
  end Master_Task;
  
  procedure Stop_Master_Task;
  
----------------------------------------------------
-- GENERIC OBSERVER TASK                           -
----------------------------------------------------
  function Exit_Observer return Boolean;
  function Observe(To_Controll : Master_Task_Access) return Boolean;
  
  package Observer is new Generic_Observer(
    Master_Task_Access,
    Exit_Observer,
    Observe
  );
  
  type My_Job_Access is access My_Job;
  
  
  
----------------------------------------------------
-- JOB ENTRY RECORD DEFINITIONS AND METHODS        -
----------------------------------------------------
  type Job_Entry_Record is record
    Job     : My_Job;
    State   : Master_Helper.Job_State := Master_Helper.Pending;
    Message : ASU.Unbounded_String;
  end record;
  
  type Job_Entry_Record_Access is access Job_Entry_Record;
  
  function "="(Left, Right : Job_Entry_Record_Access) return Boolean;
  
  package Job_Entry_Record_Vectors is new Ada.Containers.Vectors(
    Element_Type => Job_Entry_Record_Access,
    Index_Type => Positive,
    "=" => "="
  );
  
  
  function Job_Entry_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
  procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Master_Helper.Job_State; Message : String := "");
  
  
  
----------------------------------------------------
-- PROTECTED TYPE TO HANDLE JOBS                   -
----------------------------------------------------
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
  
  
  
----------------------------------------------------
-- PROTECTED TYPE TO HANDLE JOBS                   -
----------------------------------------------------
  protected Worker is
    procedure Add(New_Worker : Master_Helper.Worker_Record_Access);
    function Find_By_Access_Token_And_Type(Access_Token : String; W_Type : Utility.Worker_Type) return Master_Helper.Worker_Record_Access;
    function Find_All_By_Type(W_Type : Utility.Worker_Type) return Master_Helper.Worker_Entry_Vectors.Vector;
    procedure Print;
  private
    Worker : Master_Helper.Worker_Entry_Vectors.Vector;
  end Worker;
  
  
  
----------------------------------------------------
-- GENERIC SERVER INSTANCE                         -
----------------------------------------------------
  package Server is new Master_Server(
    My_Job,
    Job_Entry_Record_Access,
    Worker.Add,
    Worker.Find_By_Access_Token_And_Type,
    Jobs.Get_By_Id,
    Jobs.Get_Next_Pending,
    Change_Job_State,
    Job_Entry_To_Xml,
    Stop_Master_Task
  );
  
  
  
----------------------------------------------------
-- GENERIC CONSOLE INSTANCE                       --
----------------------------------------------------
  function Banner return String;
  procedure Parse_Configuration(Config_Xml : Xml.Node_Access);
  procedure Process_User_Input(User_Input : String; To_Controll : Master_Task_Access);
  
  package Console is new Generic_Console(
    Master_Task_Access,
    Banner,
    Parse_Configuration,
    Process_User_Input
  );
  
  
private
  Main_Task : Master_Task_Access;
  
end Master;