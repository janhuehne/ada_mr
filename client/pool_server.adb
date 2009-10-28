--------------------------------------------------------------
--
-- Pool_Server
--
with GNAT.Sockets ; 
use GNAT.Sockets ;
with Ada.Text_IO ;

procedure Pool_Server is

MaxTasks : constant Positive := 5 ; -- buffer size
type Index is mod MaxTasks ;

function Rev ( S : String ) return String is
Res : String ( S'Range ) ;
J : Integer := S'First ;
begin
for I in reverse S'Range loop
Res ( J ) := S ( I ) ;
J := J + 1 ;
end loop ;
return Res ;
end Rev ;

protected Aborted is
procedure Set ;
function Check return Boolean ;
private
Done : Boolean := False ;
end Aborted ;

protected body Aborted is
procedure Set is
begin
Done := True ;
end Set ;

function Check return Boolean is
begin
return Done ;
end Check ;
end Aborted ;


type Echo ;
type Echo_Access is access Echo ;

task type Echo is
entry Start ( N_Sock : IN Socket_Type ;
Self : IN Echo_Access ) ;
entry ReStart ( N_Sock : IN Socket_Type ) ;
end Echo ;

type Task_Array is array ( Index ) of Echo_Access ;

protected Buffer is
entry Deposit ( X : in Echo_Access ) ;
entry Extract ( X : out Echo_Access ) ;
function NumWaiting return Natural ;
private
Buf : Task_Array ;
I, J : Index := 0 ;
Count : Natural range 0 .. MaxTasks := 0 ;
end Buffer ;


task body Echo is
Sock : Socket_Type ;
S : Stream_Access ;
Me : Echo_Access ;
Input_Selector : Selector_Type ;
Input_Set : Socket_Set_Type ;
WSet : Socket_Set_Type ;
Input_Status : Selector_Status ;
begin
--set up selector
Create_Selector ( Input_Selector ) ;

--Initialise socket sets
--WSet is always empty as we are not interested in output events
-- RSet only ever contains one socket namely Sock
Empty ( Input_Set ) ;
Empty ( WSet ) ;

ACCEPT Start ( N_Sock : IN Socket_Type ;
Self : IN Echo_Access ) DO
Sock := N_Sock ;
Me := Self ;
end Start ;

loop
begin -- block for exception handling
S := Stream ( Sock ) ; -- set up stream on socket
Boolean'Write ( S, True ) ; -- acknowledge connection

loop
-- check for input on Sock socket
Set ( Input_Set, Sock ) ;

-- time-out on check if no input within 0.5 second
Check_Selector ( Input_Selector,
Input_Set,
WSet,
Input_Status,
0.5 ) ;
if Input_Status = Completed then
-- we have input, so process it
declare
Str : String := String'Input ( S ) ;
begin
exit when Str = "quit" ;
String'Output ( S, Rev ( Str ) ) ;
end ;
end if ;
if Aborted.Check then
String'Output ( S, "Server aborted" ) ;
exit ;
end if ;
end loop ;

Ada.Text_IO.New_Line ;
Ada.Text_IO.Put_Line ( "Slave Closing Connection" ) ;
ShutDown_Socket ( Sock, Shut_Read_Write ) ;
Buffer.Deposit ( Me ) ;

exception
-- The mostly likely exception is if client quits unexpectedly
-- close the socket and deposit ourselves in the buffer
when others =>
Ada.Text_IO.New_Line ;
Ada.Text_IO.Put_Line ( "Connection closed unexpectedly" ) ;
Close_Socket ( Sock ) ;
Buffer.Deposit ( Me ) ;
end ;

select
ACCEPT ReStart ( N_Sock : IN Socket_Type ) DO
Sock := N_Sock ;
end ReStart ;
or
-- terminate if all slaves are queued here and
-- if the main server task has finished
terminate ;
end select ;

end loop ;
end Echo ;

protected body Buffer is
entry Deposit ( X : IN Echo_Access ) when Count < MaxTasks is
begin
Buf ( I ) := X ;
I := I + 1 ;
Count := Count + 1 ;
end Deposit ;

entry Extract ( X : OUT Echo_Access ) when Count > 0 is
begin
X := Buf ( J ) ;
J := J + 1 ;
Count := Count - 1 ;
end Extract ;

function NumWaiting return Natural is
begin
return Count ;
end NumWaiting ;
end Buffer ;

Server : Socket_Type ;
New_Sock : Socket_Type ;
Slave : Echo_Access ;
Addr : Sock_Addr_Type ( Family_Inet ) ;
Peer_Addr : Sock_Addr_Type ( Family_Inet ) ;
Avail : Boolean := False ;
Ch : Character ;
TotalTasks : Natural := 0 ;
Accept_Selector : Selector_Type ;
Accept_Set : Socket_Set_Type ;
WSet : Socket_Set_Type ;
Accept_Status : Selector_Status ;

begin -- main server task
Ada.Text_IO.Put_Line ( "WARNING server loops for ever." ) ;
Ada.Text_IO.Put ( "Press A to terminate server and all " ) ;
Ada.Text_IO.Put_Line ( "tasks immediately or press Q to ") ;
Ada.Text_IO.Put ( "accept no further connections and " ) ;
Ada.Text_IO.Put ( "terminate gracefully when all clients " ) ;
Ada.Text_IO.Put ( "are fully when all clients are through." ) ;
Ada.Text_IO.New_Line ;
Initialize ;
Create_Socket ( Server) ;
Addr := ( Family_Inet,
Addresses ( Get_Host_By_Name ( Host_Name ), 1 ),
50000 ) ;
-- allow server address to be reused for multiple connections
Set_Socket_Option ( Server, 
Socket_Level,
( Reuse_Address, True ) ) ;

Bind_Socket ( Server, Addr ) ;
Listen_Socket ( Server, 4 ) ;

-- set up selector
Create_Selector ( Accept_Selector ) ;

-- Initialise socket sets
-- WSet is always empty as we are not interested in output 
-- events Accept_Set only ever contains one socket namely 
-- Server
Empty ( Accept_Set ) ;
Empty ( WSet ) ;
loop
Ada.Text_IO.Get_Immediate ( Ch, Avail ) ;
if Avail and then
( Ch = 'q' or Ch = 'Q' or Ch = 'a' or Ch = 'A' ) then
exit ;
end if ;

-- check for input (connection requests) on Server socket
Set ( Accept_Set, Server ) ;
-- time-out on check if no request within 1 second
Check_Selector ( Accept_Selector, 
Accept_Set,
WSet, 
Accept_Status, 
1.0 ) ;

if Accept_Status = Completed then
-- must be an event on Server socket as it is the only 
-- one that we are checking.
-- Hence the Accept_Socket call should not block.

Accept_Socket ( Server, New_Sock, Peer_Addr ) ;
Ada.Text_IO.New_Line ;
Ada.Text_IO.Put_Line 
( "Connection accepted -- allocating slave" ) ;


if Buffer.NumWaiting = 0 and TotalTasks < MaxTasks then
Slave := NEW Echo ; -- start new task
TotalTasks := TotalTasks + 1 ;
Ada.Text_IO.Put_Line ( "New slave task started" ) ;
-- call entry Start to activate task
Slave.Start ( New_Sock, Slave ) ;
else
Ada.Text_IO.Put_Line ( "Waiting for an idle slave task" ) ;
Buffer.Extract ( Slave ) ;
-- call entry Start to re-activate task
Slave.ReStart ( New_Sock ) ;
Ada.Text_IO.Put_Line ( " Idle slave task reactivated" ) ;
end if ;
end if ;
end loop ;

if Ch = 'a' or Ch = 'A' then
-- signal slave tasks to terminate
Aborted.Set ;
end if ;

-- tidy up
Close_Selector ( Accept_Selector ) ;
Empty ( Accept_Set ) ;

Close_Socket ( Server ) ;
Ada.Text_IO.New_Line ;
Ada.Text_IO.Put_Line ( "Main server task exiting ..." ) ;
Finalize ;
end Pool_Server ;