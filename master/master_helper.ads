with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Xml;
with GNAT.Sockets;
with Utility;

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
  function From_String(Arg : String) return Job_State;
  
  
  
----------------------------------------------------
-- WORKER RECORD                                   -
----------------------------------------------------
  type Worker_Record is record
    Identifier   : ASU.Unbounded_String;
    W_Type       : Utility.Worker_Type;
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
-- NOT DELIVERED MAP RESULT                         -
----------------------------------------------------
  type Not_Delivered_Map_Result is record
    Reducer : Worker_Record_Access;
    Result  : ASU.Unbounded_String;
  end record;
  
  type Not_Delivered_Map_Result_Access is access Not_Delivered_Map_Result;
  
  package Not_Delivered_Map_Result_Vectors is new Ada.Containers.Vectors(
    Element_Type => Not_Delivered_Map_Result_Access,
    Index_Type => Positive
  );
  
  Undelivered_Job_Results : Not_Delivered_Map_Result_Vectors.Vector;
  
  
  
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
  No_Job_Found : Exception;
  Initialization_Required : Exception;
  No_Worker_Found : Exception;
  Unknown_Job_State : Exception;
  
----------------------------------------------------
-- GLOBAL VARIABLES                                -
----------------------------------------------------
  Server_Bind_Ip   : GNAT.Sockets.Inet_Addr_Type;
  Server_Bind_Port : GNAT.Sockets.Port_Type;
  
end Master_Helper;