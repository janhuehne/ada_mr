with GNAT.Sockets;
use GNAT.Sockets;

with Ada.Text_IO;

with Xml_Helper;

package body Mapper_Helper is

  function Send_Job_Result_To_Reducer(Job_Result : String; Reducer_IP : String; Reducer_Port : String) return Boolean is
    Sock            : Socket_Type;
    S               : Stream_Access;
    Addr            : Sock_Addr_Type (Family_Inet);
    Msg             : String (1 .. 2000);
    Last            : Natural;
    B               : Boolean;
    Read_Selector   : Selector_Type;
    Read_Set, WSet  : Socket_Set_Type;
    Read_Status     : Selector_Status;
  begin
    Create_Socket(Sock);
    Addr.Addr := Addresses(Get_Host_By_Name (Reducer_IP), 1);
    Addr.Port := Port_Type'Value(Reducer_Port);
    
    Create_Selector(Read_Selector);
    Empty(Read_Set);
    Empty(WSet);
    
    Connect_Socket(Sock, Addr);
    S := Stream (Sock);
    Boolean'Read (S, B);
    
    
    Set(Read_Set, Sock);
    
    -- check for input on socket (server may be aborting)
    -- time-out immediately if no input pending
    -- We seem to need a small delay here (using zero seems to block
    -- forever)
    -- Is this a GNAT bug or AB misreading Check_Selector docs?
    Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
    
    String'Output(
      S, 
      Xml_Helper.Xml_Command(Xml_Helper.Mapper, "job_result", Job_Result)
    );
    
    declare
      Str : String := String'Input(S);
    begin
      Ada.Text_IO.Put_Line(Str);
    end;
    
    ShutDown_Socket(Sock);
    Close_Selector(Read_Selector);
    
    return true;
  exception 
    when others => return false;
  end Send_Job_Result_To_Reducer;
  
end Mapper_Helper;