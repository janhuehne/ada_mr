with Ada.Containers.Vectors;
with Runner;
with Xml;

generic
  with function Merge_Jobs return Boolean;
  with function Finalize return Boolean;
  
package Reducer is
  
  package Runner_MR is new Runner(Merge_Jobs, Finalize);
  
  type Reducer_Task;
  type Reducer_Task_Access is access Reducer_Task;
  
  task type Reducer_Task is
    entry Start;
    entry Stop;
  end Reducer_Task;
  
  task type Console is
    entry Start(C_Arg : Reducer_Task_Access);
  end Console;
  
end Reducer;