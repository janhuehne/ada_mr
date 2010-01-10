with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Xml;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;

generic
  with procedure Process_Request(S : Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access);
  
package Ada_Mr.Generics.Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  Max_Tasks : CONSTANT Positive := 5; --Positive'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TASKS"));
  type Index is mod Max_Tasks;
  
  type Echo; 
  type Echo_Access is access Echo;
  
  task type Echo is
    entry Start(N_Sock : IN Socket_Type; Self : IN Echo_Access);
    entry ReStart (N_Sock : IN Socket_Type);
  end Echo;
  
  type Task_Array is array(Index) of Echo_Access;
  
  protected Buffer is
    entry Deposit(X : Echo_Access);
    entry Extract(X : out Echo_Access);
    function Num_Waiting return Natural;
  private
     Buf : Task_Array;
     I, J : Index := 0;
     Count : Natural range 0 .. Max_Tasks := 0;
  end Buffer;
  
end Ada_Mr.Generics.Echo;