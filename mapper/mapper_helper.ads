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
  
  
  -- Mapper config variables
  Identifier       : ASU.Unbounded_String;
  Listen_Sock_Addr : GNAT.Sockets.Sock_Addr_Type;
  Master_Sock_Addr : GNAT.Sockets.Sock_Addr_Type;
  
  
  
  
end Mapper_Helper;