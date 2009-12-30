with Ada.Containers.Vectors;
with Mapper_Runner;
with Xml;
with GNAT.Sockets;
with Mapper_Server;
with Ada.Strings.Unbounded;
with Generic_Console;
with Generic_Observer;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Print_Job(Job : in My_Job; State : String);
  with procedure Compute_Job(Job : in My_Job);
  with function Job_Result_To_Xml return String;
  
package Mapper is

----------------------------------------------------
-- PACKAGE RENAMES                                 -
----------------------------------------------------
  package ASU renames Ada.Strings.Unbounded;



----------------------------------------------------
-- RUNNER PACKAGE                                 --
----------------------------------------------------
  package Runner is new Mapper_Runner(
    My_Job, 
    From_Xml, 
    To_Xml,
    Get_Job_Id,
    Compute_Job, 
    Job_Result_To_Xml
  );



----------------------------------------------------
-- MAPPER TASK                                    --
----------------------------------------------------
  type Mapper_Task;
  type Mapper_Task_Access is access Mapper_Task;
  
  task type Mapper_Task is
    entry Start(Self : Mapper_Task_Access; Config_File : String);
    entry Stop;
  end Mapper_Task;
  
  procedure Stop_Mapper_Task;


----------------------------------------------------
-- GENERIC OBSERVER TASK                          --
----------------------------------------------------
  function Exit_Observer return Boolean;
  function Observe(To_Controll : Mapper_Task_Access) return Boolean;
  
  package Observer is new Generic_Observer(
    Mapper_Task_Access,
    Exit_Observer,
    Observe
  );



----------------------------------------------------
-- SERVER PACKAGE                                 --
----------------------------------------------------
  package Server is new Mapper_Server(
    Stop_Mapper_Task
  );



----------------------------------------------------
-- Global configuration variables                 --
----------------------------------------------------
  Listen_On_Port : GNAT.Sockets.Port_Type := 7100;



----------------------------------------------------
-- GENERIC CONSOLE METHODS                        --
----------------------------------------------------
  function Banner return String;
  procedure Parse_Configuration(Config_Xml : Xml.Node_Access);
  
--  package Console is new Generic_Console(
--    Mapper_Task_Access,
--    Banner,
--    Parse_Configuration,
--    Process_User_Input
--  );
private
  Main_Task : Mapper_Task_Access;
end Mapper;