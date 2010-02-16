with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Ada_Mr.Xml;
with Ada_Mr.Master.Helper;
with Ada_Mr.Master.Server;
with Ada_Mr.Generics.Console;
  with Ada_Mr.Generics.Runner;

with Ada_Mr.Helper;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with procedure Set_Job_Id(The_Job : in out My_Job);
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String; Message : String);
  with procedure Split_Raw_Data;
  with function Get_Next_Raw_Job return My_Job;
  
package Ada_Mr.Master.Main is
  
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
    entry Start(Self : Master_Task_Access);
    entry Stop;
  end Master_Task;
  
  procedure Stop_Master_Task;
  
----------------------------------------------------
-- GENERIC OBSERVER TASK                           -
----------------------------------------------------
  function Exit_Observer return Boolean;
  procedure Observe;
  
  package Observer is new Ada_Mr.Generics.Runner(
    Observe
  );
  
  type My_Job_Access is access My_Job;
  
  
  
----------------------------------------------------
-- JOB ENTRY RECORD DEFINITIONS AND METHODS        -
----------------------------------------------------
  type Job_Entry_Record is record
    Job     : My_Job;
    State   : Ada_Mr.Master.Helper.Job_State := Ada_Mr.Master.Helper.Pending;
    Message : ASU.Unbounded_String := ASU.To_Unbounded_String("");
  end record;
  
  type Job_Entry_Record_Access is access Job_Entry_Record;
  
  function "="(Left, Right : Job_Entry_Record_Access) return Boolean;
  
  package Job_Entry_Record_Vectors is new Ada.Containers.Vectors(
    Element_Type => Job_Entry_Record_Access,
    Index_Type => Positive,
    "=" => "="
  );
  
  
  function Job_Entry_To_Xml(Job_Entry : Job_Entry_Record_Access) return String;
  procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Ada_Mr.Master.Helper.Job_State; Message : String := "");
  
  
  
----------------------------------------------------
-- PROTECTED TYPE TO HANDLE JOBS                   -
----------------------------------------------------
  protected Jobs is
    procedure Add(Job : My_Job);
    function Get_By_Id(Id : Natural) return Job_Entry_Record_Access;
    function Get_Next_Pending return Job_Entry_Record_Access;
    function Count return Natural;
    function Count_By_State(State : Ada_Mr.Master.Helper.Job_State) return Natural;
    procedure Print;
  private
    Jobs : Job_Entry_Record_Vectors.Vector;
  end Jobs;
  
  
  
----------------------------------------------------
-- PROTECTED TYPE TO HANDLE JOBS                   -
----------------------------------------------------
  protected Worker is
    procedure Add(New_Worker : in out Ada_Mr.Master.Helper.Worker_Record_Access);
    procedure Stop_All;
    function Exists_Identifier(Identifier : String) return Boolean;
    function Find_By_Identifier(Identifier : String) return Ada_Mr.Master.Helper.Worker_Record_Access;
    function Find_By_Access_Token_And_Type(Access_Token : String; W_Type : Ada_Mr.Helper.Worker_Type) return Ada_Mr.Master.Helper.Worker_Record_Access;
    function Find_All_By_Type(W_Type : Ada_Mr.Helper.Worker_Type) return Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector;
    procedure Print;
  private
    Worker : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector;
    
    Mapper_Counter  : Natural := 1;
    Reducer_Counter : Natural := 1;
  end Worker;
  
  
  
----------------------------------------------------
-- GENERIC SERVER INSTANCE                         -
----------------------------------------------------
  package Server is new Ada_Mr.Master.Server(
    My_Job,
    Job_Entry_Record_Access,
    From_Xml,
    Worker.Add,
    Worker.Find_By_Identifier,
    Worker.Find_By_Access_Token_And_Type,
    Jobs.Add,
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
--  procedure Parse_Configuration(Config_Xml : Ada_Mr.Xml.Node_Access);
  procedure Process_User_Input(User_Input : String; To_Controll : Master_Task_Access);
  
  package Console is new Ada_Mr.Generics.Console(
    Master_Task_Access,
    Banner,
    Process_User_Input
  );
  
  
private
  Main_Task : Master_Task_Access;
  
end Ada_Mr.Master.Main;