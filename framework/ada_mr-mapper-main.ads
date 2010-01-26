with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

with Ada_Mr.Mapper.Runner;
with Ada_Mr.Mapper.Server;
with Ada_Mr.Xml;



--with Ada_Mr.Generics.Console;
--with Ada_Mr.Generics.Observer;
with Ada_Mr.Helper;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Compute_Job(Job : in My_Job);
  with function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  
package Ada_Mr.Mapper.Main is

----------------------------------------------------
-- PACKAGE RENAMES                                 -
----------------------------------------------------
  package ASU renames Ada.Strings.Unbounded;

  procedure Stop_Mapper_Task;
  procedure Abort_Mapper_Task;


----------------------------------------------------
-- RUNNER PACKAGE                                 --
----------------------------------------------------
  package Runner is new Ada_Mr.Mapper.Runner(
    My_Job, 
    From_Xml, 
    To_Xml,
    Get_Job_Id,
    Compute_Job, 
    Split_Result_For_Different_Reducer,
    Stop_Mapper_Task
  );



----------------------------------------------------
-- MAPPER TASK                                    --
----------------------------------------------------
  type Mapper_Task;
  type Mapper_Task_Access is access Mapper_Task;
  
  task type Mapper_Task is
    entry Start(Self : Mapper_Task_Access);
    entry Stop;
    entry Abort_It;
  end Mapper_Task;



----------------------------------------------------
-- GENERIC OBSERVER TASK                          --
----------------------------------------------------
--  function Exit_Observer return Boolean;
--  function Observe(To_Controll : Mapper_Task_Access) return Boolean;
  
--  package Observer is new Ada_Mr.Generics.Observer(
--    Mapper_Task_Access,
--    Exit_Observer,
--    Observe
--  );



----------------------------------------------------
-- SERVER PACKAGE                                 --
----------------------------------------------------
  package Server is new Ada_Mr.Mapper.Server(
    Stop_Mapper_Task,
    Abort_Mapper_Task
  );



----------------------------------------------------
-- Global configuration variables                 --
----------------------------------------------------
  Listen_On_Port : GNAT.Sockets.Port_Type := 7100;



----------------------------------------------------
-- GENERIC CONSOLE METHODS                        --
----------------------------------------------------
  function Banner return String;
--  procedure Parse_Configuration(Config_Xml : Ada_Mr.Xml.Node_Access);
  
--  package Console is new Ada_Mr.Generics.Console(
--    Mapper_Task_Access,
--    Banner,
--    Parse_Configuration,
--    Process_User_Input
--  );
private
  Main_Task : Mapper_Task_Access;
end Ada_Mr.Mapper.Main;