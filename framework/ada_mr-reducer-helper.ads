  with Ada_Mr.Xml;
with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Ada_Mr.Reducer.Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  protected Aborted is
    procedure Stop;
    function Check return Boolean;
  private
    Abort_It  : Boolean := false;
  end Aborted;
  
  procedure Import_Not_Delivered_Mapper_Results_From_Master;
  
  
  protected Mapper_Results is
    procedure Add(Node : Ada_Mr.Xml.Node_Access);
    procedure Add(Node_Vector : Ada_Mr.Xml.Node_Access_Vector.Vector);
    function Is_Empty return Boolean;
    function Get_Next return Ada_Mr.Xml.Node_Access;
--  private
--    Mapper_Results_Vector : Ada_Mr.Xml.Node_Access_Vector.Vector;
  end Mapper_Results;
  
  Mapper_Results_Vector : Ada_Mr.Xml.Node_Access_Vector.Vector;
  
--  Index : Positive := 1;
  
  
end Ada_Mr.Reducer.Helper;