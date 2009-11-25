with Ada.Containers.Vectors;
with Xml;
with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Reducer_Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
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
  
----------------------------------------------------
-- GLOBAL VARIABLES                               --
----------------------------------------------------
  Identifier       : ASU.Unbounded_String;
  Access_Token     : String(1..32) := "no initialized yet              ";
  
  Server_Bind_Ip   : GNAT.Sockets.Inet_Addr_Type;
  Server_Bind_Port : GNAT.Sockets.Port_Type;
  
end Reducer_Helper;