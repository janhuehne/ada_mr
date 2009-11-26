package body Utility is
  
  function Starts_With(Item : String; Pattern : String; Ignore_Case : Boolean := false) return Boolean is
  begin
    return Is_Equal(Item(Pattern'First .. Pattern'Last), Pattern, Ignore_Case);
  exception
    when others => return false;
  end Starts_With;
  
  
  function Is_Equal(Arg_1 : String; Arg_2 : String; Ignore_Case : Boolean := false) return Boolean is
    String_1 : String := Arg_1;
    String_2 : String := Arg_2;
  begin
    
    if Ignore_Case = true then
      String_1 := To_Lower(String_1);
      String_2 := To_Lower(String_2);
    end if;
    
    if String_1'Length = String_2'Length then
      if String_1 = String_2 then
        return true;
      end if;
    end if;
    
    return false;
  exception
    when others => return false;
  end Is_Equal;
  
  
  function Is_Equal(Item : String; Input_Length : Natural; Pattern : String; Ignore_Case : Boolean := false) return Boolean is
  begin
    return Is_Equal(Item(Item'First .. Input_Length), Pattern, Ignore_Case);
  exception
    when others => return false;
  end Is_Equal;
  
  
  function Is_Equal(Arg_1 : String; Arg_2 : Ada.Strings.Unbounded.Unbounded_String; Ignore_Case : Boolean := false) return Boolean is
  begin
    return Is_Equal(Arg_1, Ada.Strings.Unbounded.To_String(Arg_2), Ignore_Case);
  exception
    when others => return false;
  end Is_Equal;
  
  
  procedure Put(Str : String; Field_Length : Natural := 0; Space_Pos : Natural := 1) is
    
    procedure Print_Spaces is
      Space_Counter : Natural := 0;
    begin
      loop
        exit when Field_Length - Str'Length = Space_Counter;
          Ada.Text_IO.Put(" ");
          Space_Counter := Space_Counter + 1;
      end loop;
    end Print_Spaces;
    
  begin
    if Space_Pos = 1 then
      Print_Spaces;
    end if;
      
    Ada.Text_IO.Put(Str);
    
    if Space_Pos = 2 then
      Print_Spaces;
    end if;

  end Put;
  
  
  procedure Put_Line(Str : String; Field_Length : Natural := 0; Space_Pos : Natural := 1) is
  begin
    Put(Str, Field_Length, Space_Pos);
    Ada.Text_IO.New_Line;
  end Put_Line;
  
  
  function Does_File_Exist (Name : String) return Boolean is
    The_File : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Name);
    Ada.Text_IO.Close (The_File);
    return True;
  exception
    when Ada.Text_IO.Name_Error => return False;
  end Does_File_Exist;
  
  
  procedure Print_Exception(Error : Ada.Exceptions.Exception_Occurrence; Message : String := "") is
  begin
    Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(Error));
    Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(Error));
    Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(Error));
  end Print_Exception;
  
  
  function Send(Host : String; Port : GNAT.Sockets.Port_Type; Command : String) return String is
    use GNAT.Sockets;
    
    Addr : Sock_Addr_Type(Family_Inet);
  begin
    Addr.Addr := Addresses(Get_Host_By_Name (Host), 1);
    Addr.Port := Port;
    
    return Send(Addr, Command);
  end Send;
  
  
  function Send(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type; Command : String) return String is
    use GNAT.Sockets;
  
    Addr : Sock_Addr_Type(Family_Inet);
  begin
    Addr.Addr := Host;
    Addr.Port := Port;
    
    return Send(Addr, Command);
  end Send;
  
  
  function Send(Addr : GNAT.Sockets.Sock_Addr_Type; Command : String) return String is
    use GNAT.Sockets;
    
    Sock            : Socket_Type;
    S               : Stream_Access;
    B               : Boolean;
    Read_Selector   : Selector_Type;
    Read_Set, WSet  : Socket_Set_Type;
    Read_Status     : Selector_Status;
  begin
    Initialize;
    Create_Socket(Sock);
    
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
      Command
    );
    
    declare
      Str : String := String'Input(S);
    begin
      Ada.Text_IO.Put_Line("Response: " & Str);
--      ShutDown_Socket(Sock);
      Close_Selector(Read_Selector);
      Finalize;
      
      return Str;
    end;
  exception
    when Error : others =>
--      ShutDown_Socket(Sock);
      Close_Selector(Read_Selector);
      Finalize;
      
      raise;
  end Send;
end Utility;