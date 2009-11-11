with Ada.Containers.Vectors;
with Xml;

package Reducer_Helper is
  
  protected Aborted is
    procedure Stop;
    function Check return Boolean;
  private
    Abort_It  : Boolean := false;
  end Aborted;
  
  package Xml_Node_Access_Vectors is new Ada.Containers.Vectors(
    Element_Type => Xml.Node_Access, 
    Index_Type => Positive,
    "=" => Xml."="
  );
  
  Finished_Jobs_Queue : Xml_Node_Access_Vectors.Vector;
  
end Reducer_Helper;