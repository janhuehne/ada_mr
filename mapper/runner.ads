with GNAT.Sockets;
use GNAT.Sockets;
with Xml;
--with Mapper;


generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Compute_Job(Job : in My_Job) return Boolean;
  with function Job_Result_To_Xml return String;
    
package Runner is  
  protected Aborted is
    procedure Stop;
    function Check return Boolean;
  private
    Abort_It  : Boolean := false;
  end Aborted;
  
  task type Runner_Task is
    entry Start;
    entry Stop;
  end Runner_Task;
  
  
  
  
end Runner;
