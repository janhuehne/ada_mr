with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Mapper_Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  protected Aborted is
    procedure Set_Abort;
    procedure Set_Exit;
    function Get_Abort return Boolean;
    function Get_Exit return Boolean;
  private
    Abort_Mapper  : Boolean := false;
    Exit_Mapper   : Boolean := false;
  end Aborted;
  
  
  Reducer_Not_Found : Exception;
  
  
  ----------------------------------------------------
  -- GLOBAL VARIABLES                                -
  ----------------------------------------------------
  Identifier       : ASU.Unbounded_String;
  Access_Token     : String(1..32) := "no initialized yet              ";
  
  Server_Bind_Ip   : GNAT.Sockets.Inet_Addr_Type;
  Server_Bind_Port : GNAT.Sockets.Port_Type;
  
  Master_Ip   : GNAT.Sockets.Inet_Addr_Type;
  Master_Port : GNAT.Sockets.Port_Type;
  
  Hmac_Passphrase : ASU.Unbounded_String;
end Mapper_Helper;