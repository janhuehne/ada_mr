with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Xml;
with GNAT.Sockets;

package Master_Helper is

----------------------------------------------------
-- PACkAGE RENAMES                                 -
----------------------------------------------------
  package ASU renames Ada.Strings.Unbounded;
  
  
  
----------------------------------------------------
-- TYPE DEFINITIONS AND FUNCTIONS                  -
----------------------------------------------------
  type Job_State is (
    Pending,
    In_Progress,
    Done
  );
  
  function To_String(Arg : Job_State) return String;
  
  
  type Worker_Type is (
    Mapper,
    Reducer,
    Invalid
  );
  
  function String_To_Worker_Type(Arg : String) return Worker_Type;
  function To_String(Arg : Worker_Type) return String;
  
  
  
----------------------------------------------------
-- WORKER RECORD                                   -
----------------------------------------------------
  type Worker_Record is record
    Identifier   : ASU.Unbounded_String;
    W_Type       : Worker_Type;
    Ip           : GNAT.Sockets.Inet_Addr_Type;
    Port         : GNAT.Sockets.Port_Type;
    Access_Token : String(1..32);
  end record;
  
  type Worker_Record_Access is access Worker_Record;
  
  package Worker_Entry_Vectors is new Ada.Containers.Vectors(
    Element_Type => Worker_Record_Access,
    Index_Type => Positive
  );
  
  
  
----------------------------------------------------
-- PROTECTED TYPE TO HANDLE ABORT  MESSAGES        -
----------------------------------------------------
  protected Aborted is
    procedure Set_Abort;
    procedure Set_Exit;
    function Get_Abort return Boolean;
    function Get_Exit return Boolean;
  private
    Abort_Master  : Boolean := false;
    Exit_Master   : Boolean := false;
  end Aborted;
  
  
  
----------------------------------------------------
-- EXCEPTION DEFINITIONS                           -
----------------------------------------------------
  Unknow_Worker_Type : Exception;
  No_Job_Found : Exception;
  Initialization_Required : Exception;
  
  
  
----------------------------------------------------
-- GLOBAL VARIABLES                                -
----------------------------------------------------
  Server_Bind_Ip   : GNAT.Sockets.Inet_Addr_Type;
  Server_Bind_Port : GNAT.Sockets.Port_Type;
  
  Reducer_Ip   : GNAT.Sockets.Inet_Addr_Type;
  Reducer_Port : GNAT.Sockets.Port_Type;
  
end Master_Helper;