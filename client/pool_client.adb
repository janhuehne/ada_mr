--------------------------------------------------------------
--
-- Pool_Client
--
with Gnat.Sockets ;
use Gnat.Sockets ;
with Ada.Command_Line ; 
use Ada.Command_Line ;
with Ada.Text_IO ;
use Ada.Text_IO ;

procedure Pool_Client is
Sock : Socket_Type ;
S : Stream_Access ;
Addr : Sock_Addr_Type ( Family_Inet ) ;
Msg : String ( 1 .. 2000 ) ;
Last : Natural ;
B : Boolean ;
Read_Selector : Selector_Type ;
Read_Set, WSet : Socket_Set_Type ;
Read_Status : Selector_Status ;
begin
Initialize ;
Create_Socket ( Sock ) ;
--Addr := ( Family_Inet,
--Addresses ( Get_Host_By_Name ( Argument ( 1 ) ), 1 ),
--50000 ) ;

Addr.Addr := Addresses(Get_Host_By_Name ("127.0.0.1"), 1);
Addr.Port := 7000;

Create_Selector ( Read_Selector ) ;
Empty ( Read_Set ) ;
Empty ( WSet ) ;


Connect_Socket ( Sock, Addr ) ;
S := Stream ( Sock ) ;
Boolean'Read ( S, B ) ;
-- wait for connection to be accepted

loop
Set ( Read_Set, Sock ) ;

-- check for input on socket (server may be aborting)
-- time-out immediately if no input pending
-- We seem to need a small delay here (using zero seems to block
-- forever)
-- Is this a GNAT bug or AB misreading Check_Selector docs?

Check_Selector ( Read_Selector, 
Read_Set,
WSet, 
Read_Status,
0.005 ) ;
if Read_Status = Expired then
Ada.Text_IO.Put ( "Message> " ) ; -- prompt user for message
Ada.Text_IO.Get_Line ( Msg, Last ) ;

-- send message to socket unless server is aborting
String'Output ( S, Msg ( 1 .. Last ) ) ;
exit when Msg ( 1 .. Last ) = "quit" ;
end if ;

declare 
-- receive message
Str : String := String'Input ( S ) ;
begin Ada.Text_IO.Put_Line ( Str ) ;
exit when Str = "Server aborted" ;
end ;
end loop ;

Ada.Text_IO.Put_Line ( "Client quitting ..." ) ;
ShutDown_Socket ( Sock ) ;
Close_Selector ( Read_Selector ) ;
Finalize ;
exception 
when others =>
Ada.Text_IO.Put_Line ("Exception: Client quitting ..." ) ;
Close_Socket ( Sock ) ;
Close_Selector( Read_Selector ) ;
Finalize ;
end Pool_Client ;
