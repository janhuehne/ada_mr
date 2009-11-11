with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Xml is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Node;
  type Node_Access is access Node;
  
  Node_Not_Found : exception;
  
  package Node_Access_Vector is new Ada.Containers.Vectors(
    Element_Type => Node_Access, 
    Index_Type => Positive);
    
  type Node is record
    Tag      : ASU.Unbounded_String;
    Value    : ASU.Unbounded_String;
    Parent   : Node_Access;
    Children : Node_Access_Vector.Vector;
  end record;
  
  procedure Add_Node(Root_Node : in out Node_Access; Child : in out Node_Access);
  procedure Print(Root_Node : in Node_Access; Depth : Integer := 0);
    
  function Find_Child_With_Tag(Root_Node : in Node_Access; Tag : in String) return Node_Access;
  function Get_Value(Root : Node_Access; Tag : String) return String;
  function Get_Tag(Root : Node_Access) return String;
  
  function "="(Left, Right : Xml.Node_Access) return Boolean;
  
  Wrong_Xml : Exception;
end Xml;