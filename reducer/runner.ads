with GNAT.Sockets;
use GNAT.Sockets;
with Xml;
with Echo;


generic
  with function Merge_Jobs(Xml_Node : Xml.Node_Access) return Boolean;
  with function Finalize_Jobs return Boolean;
    
package Runner is
  
  package Echo_MR is new Echo(Merge_Jobs, Finalize_Jobs);
  
  task type Runner_Task is
    entry Start;
    entry Stop;
  end Runner_Task;
  
  task type Result_Merge_Task is
    entry Start;
  end Result_Merge_Task;
  
end Runner;
