-------------------------------------------------------------- 
-- 
--  Pool_Client 
-- 
with Gnat.Sockets;
use  Gnat.Sockets;
with Ada.Command_Line;
use  Ada.Command_Line;
with Ada.Text_IO;
use  Ada.Text_IO;
with Xml;
with Xml_Parser;
with Workers;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

procedure Pool_Client is 
    Sock : Socket_Type;
    S : Stream_Access;
    Addr : Sock_Addr_Type;
    Msg : String ( 1 .. 80 );
    Last : Natural;
    B : Boolean;
    Read_Selector : Selector_Type;
    Read_Set, WSet : Socket_Set_Type;
    Read_Status : Selector_Status;
    
    Config_Xml : Xml.Node_Access;
  begin
    
    Config_Xml := Xml_Parser.Parse(File_Name => Ada.Command_Line.Argument(1));
    --Xml.Print(Config_Xml);
    
    -- Check xml context
    if Ada.Strings.Unbounded.To_String(Config_Xml.Tag) /= "adamr_client_config" then
      Ada.Exceptions.Raise_Exception(Xml.Wrong_Xml'Identity, "ERROR! Clientkonfigdatei konnte nicht korrekt verarbeitet werden!");
    end if;
    
    -- initial things
    Initialize;
    Create_Socket (Sock);
    
    -- Read xml data and set address details
    Addr.Addr := Addresses(Get_Host_By_Name(Ada.Strings.Unbounded.To_String(Xml.Find_Child_With_Tag(Config_Xml, "master_address").value)), 1);
    Addr.Port := Port_Type'Value(Ada.Strings.Unbounded.To_String(Xml.Find_Child_With_Tag(Config_Xml, "master_port").value));
    
    Create_Selector(Read_Selector);
    Empty(Read_Set); 
    Empty(WSet);
    Connect_Socket(Sock, Addr);
    S := Stream(Sock);
    Boolean'Read(S, B);
    -- wait for connection to be accepted 
    
    
    
    
    -- say hello to the master server
    String'Output(S, "<?xml version=""1.0"" ?><adamr-client><client-id>232132</client-id><client-type>Mapper</client-type></adamr-client>");
    
    
    loop 
      Ada.Text_IO.Put(".");
      Set ( Read_Set, Sock ) ; 
      -- check for input on socket  (server may be aborting) 
      -- time-out immediately if no input pending 
      -- We seem to need a small delay here (using zero seems to block 
      -- forever) 
      -- Is this a GNAT bug or AB misreading Check_Selector docs? 
      Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
      
      if Read_Status = Expired then 
        Ada.Text_IO.Put ( "Message> " ) ;  -- prompt user for message 
        Ada.Text_IO.Get_Line ( Msg, Last ) ; 
        -- send message to socket unless server is aborting 
        String'Output ( S, Msg ( 1 .. Last ) ) ; 
        exit when Msg ( 1 .. Last ) = "quit" ; 
      end if ; 
      declare 
          -- receive message 
          Str : String := String'Input ( S ) ; 
      begin      Ada.Text_IO.Put_Line ( Str ) ; 
        exit when Str = "Server aborted" ; 
      end ; 
    end loop ; 
    Ada.Text_IO.Put_Line ( "Client quitting ..." ) ; 
    ShutDown_Socket ( Sock ) ; 
    Close_Selector ( Read_Selector ) ; 
    Finalize ; 
--  exception 
--    when others => 
--        Ada.Text_IO.Put_Line ("Exception: Client quitting ..." ) ; 
--        Close_Socket ( Sock ) ; 
--        Close_Selector( Read_Selector ) ; 
--        Finalize ; 
  end Pool_Client ;