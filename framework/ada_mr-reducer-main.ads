with Ada_Mr;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
--with Runner;
with Ada_Mr.Xml;

with Ada_Mr.Generics.Console;

with Ada_Mr.Reducer.Server;
with Ada_Mr.Reducer.Runner;
with Ada_Mr.Generics.Runner;

generic
  with procedure Merge_Jobs(Xml_Node : Ada_Mr.Xml.Node_Access);
  with procedure Finalize_Jobs;
  
package Ada_Mr.Reducer.Main is
  
  package ASU renames Ada.Strings.Unbounded;


----------------------------------------------------
-- REDUCER TASK                                   -
----------------------------------------------------
  type Reducer_Task;
  type Reducer_Task_Access is access Reducer_Task;
  
  task type Reducer_Task is
    entry Start(Self : Reducer_Task_Access);
    entry Stop;
  end Reducer_Task;

  procedure Stop_Reducer_Task;



----------------------------------------------------
-- RESULT MERGE TASK                               -
----------------------------------------------------
  procedure Merge_Mapper_Results;
    
  package Result_Merge is new Ada_Mr.Generics.Runner(
    Merge_Mapper_Results
  );




----------------------------------------------------
-- GENERIC SERVER INSTANCE                        --
----------------------------------------------------
  package Server is new Ada_Mr.Reducer.Server(
    Finalize_Jobs,
    Stop_Reducer_Task
  );



----------------------------------------------------
-- RUNNER INSTANCE                                --
----------------------------------------------------
  package Runner is new Ada_Mr.Reducer.Runner(
    Stop_Reducer_Task
  );



----------------------------------------------------
-- GENERIC CONSOLE INSTANCE                       --
----------------------------------------------------
  function Banner return String;
--  procedure Parse_Configuration(Config_Xml : Ada_Mr.Xml.Node_Access);
  
  
  
    
  Main_Task : Reducer_Task_Access;
  
  

end Ada_Mr.Reducer.Main;