with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
--with Runner;
with Xml;
with Reducer_Server;
with Generic_Console;

generic
  with function Merge_Jobs(Xml_Node : Xml.Node_Access) return Boolean;
  with procedure Finalize_Jobs;
  
package Reducer is
  
  package ASU renames Ada.Strings.Unbounded;


----------------------------------------------------
-- REDUCER TASK                                   -
----------------------------------------------------
  type Reducer_Task;
  type Reducer_Task_Access is access Reducer_Task;
  
  task type Reducer_Task is
    entry Start;
    entry Stop;
  end Reducer_Task;


----------------------------------------------------
-- GENERIC SERVER INSTANCE                        --
----------------------------------------------------
  package Server is new Reducer_Server(
    Finalize_Jobs
  );


----------------------------------------------------
-- GENERIC CONSOLE INSTANCE                       --
----------------------------------------------------
  function Banner return String;
  procedure Parse_Configuration(Config_Xml : Xml.Node_Access);
  procedure Process_User_Input(User_Input : String; To_Controll : Reducer_Task_Access);

  package Console is new Generic_Console(
    Reducer_Task_Access,
    Banner,
    Parse_Configuration,
    Process_User_Input
  );




end Reducer;