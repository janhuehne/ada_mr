with GNAT.Sockets;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;

package Ada_Mr.Mapper.Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  protected Aborted is
    procedure Stop;
    function Check return Boolean;
  private
    Abort_It  : Boolean := false;
  end Aborted;
  
  procedure Send_Result(Reducer_Result_Map : Ada_Mr.Helper.String_String_Maps.Map);
  
  Reducer_Not_Found : Exception;
end Ada_Mr.Mapper.Helper;