with GNAT.Sockets;
use GNAT.Sockets;

with Mapper;

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
