with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Mapper_Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  protected Aborted is
    procedure Stop;
    function Check return Boolean;
  private
    Abort_It  : Boolean := false;
  end Aborted;
  
  Reducer_Not_Found : Exception;
end Mapper_Helper;