with Ada.Containers.Vectors;
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
  
  package Xml_Node_Access_Vectors is new Ada.Containers.Vectors(
    Element_Type => Ada_Mr.Xml.Node_Access, 
    Index_Type => Positive,
    "=" => Ada_Mr.Xml."="
  );
  
  Finished_Jobs_Queue : Xml_Node_Access_Vectors.Vector;
  
end Ada_Mr.Reducer.Helper;