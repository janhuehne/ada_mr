with GNAT.Sockets;
use GNAT.Sockets;
with Xml;
with Echo;


generic
  with function Merge_Jobs return Boolean;
  with function Finalize return Boolean;
    
package Runner is
  
  package Echo_MR is new Echo(Merge_Jobs, Finalize);
  
  task type Runner_Task is
    entry Start;
    entry Stop;
  end Runner_Task;
  
end Runner;
