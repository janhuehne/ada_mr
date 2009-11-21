with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Xml;
with GNAT.Sockets;

package Master_Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Job_State is (Pending, In_Progress, Done);
  type Worker_Type is (Mapper, Reducer, Invalid);
    
  protected Aborted is
    procedure Stop_Master;
    procedure Stop_Clients;
    function Check_Master return Boolean;
    function Check_Clients return Boolean;
  private
    Abort_Master  : Boolean := false;
    Abort_Clients : Boolean := false;
  end Aborted;
  
  type Worker_Record is record
    Identifier   : ASU.Unbounded_String;
    W_Type       : Worker_Type;
    Ip           : GNAT.Sockets.Inet_Addr_Type;
    Listen_Port  : GNAT.Sockets.Port_Type;
    Access_Token : String(1..32);
    
  end record;
  
  type Worker_Record_Access is access Worker_Record;
  
  package Worker_Entry_Vectors is new Ada.Containers.Vectors(
    Element_Type => Worker_Record_Access,
    Index_Type => Positive
  );
  
  Unknow_Worker_Type : Exception;
  No_Job_Found : Exception;
  Initialization_Required : Exception;
  
  function String_To_Worker_Type(Arg : String) return Worker_Type;
  function To_String(Arg : Worker_Type) return String;
  function To_String(Arg : Job_State) return String;
  
end Master_Helper;