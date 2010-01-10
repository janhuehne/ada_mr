with Logger;
with Ada.Strings.Fixed;
with Crypto_Helper;
with Xml_Parser;

package body Application_Helper is
  
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
    
    Field : Positive := Field_Length;
    
    procedure Print_Spaces is
      Space_Counter : Natural := 0;
    begin
      loop
        exit when Field - Str'Length = Space_Counter;
          Ada.Text_IO.Put(" ");
          Space_Counter := Space_Counter + 1;
      end loop;
    end Print_Spaces;
    
  begin
    if Str'Length > Field then
      Field := Str'Length + 1;
    end if;
      
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
    if Message /= "" then
      Logger.Put_Line(Message, Logger.Info);
      Logger.Put_Line("-----------------------------------------------", Logger.Info);
    end if;
    
    Logger.Put_Line(Ada.Exceptions.Exception_Message(Error) & " (" & Ada.Exceptions.Exception_Name(Error) & ")", Logger.Err);
  end Print_Exception;
  
  
  function Send(Host : String; Port : String; Command : String; Tries : Natural := 1; Wait_Between_Tries : Natural := 5) return String is
    use GNAT.Sockets;
    
    Addr : Sock_Addr_Type(Family_Inet);
  begin
    Addr.Addr := Addresses(Get_Host_By_Name (Host), 1);
    Addr.Port := Port_Type'Value(Port);
    
    if Tries = 1 then
      return Send(Addr, Command);
    else
      return Send(Addr, Command, Tries, Wait_Between_Tries);
    end if;
  end Send;
  
  
  function Send(Host : String; Port : GNAT.Sockets.Port_Type; Command : String; Tries : Natural := 1; Wait_Between_Tries : Natural := 5) return String is
    use GNAT.Sockets;
    
    Addr : Sock_Addr_Type(Family_Inet);
  begin
    Addr.Addr := Addresses(Get_Host_By_Name (Host), 1);
    Addr.Port := Port;
    
    if Tries = 1 then
      return Send(Addr, Command);
    else
      return Send(Addr, Command, Tries, Wait_Between_Tries);
    end if;
  end Send;
  
  
  function Send(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type; Command : String; Tries : Natural := 1; Wait_Between_Tries : Natural := 5) return String is
    use GNAT.Sockets;
  
    Addr : Sock_Addr_Type(Family_Inet);
  begin
    Addr.Addr := Host;
    Addr.Port := Port;
    
    if Tries = 1 then
      return Send(Addr, Command);
    else
      return Send(Addr, Command, Tries, Wait_Between_Tries);
    end if;
  end Send;
  
  function Send(Addr : GNAT.Sockets.Sock_Addr_Type; Command : String; Tries : Natural; Wait_Between_Tries : Natural := 5) return String is
    Response : ASU.Unbounded_String;
  begin
    for I in 1 .. Tries loop
      declare
      begin
        Response := ASU.To_Unbounded_String(Send(Addr, Command));
        exit;
      exception
        when GNAT.Sockets.Socket_Error =>
          Logger.Put_Line("Attempt " & I'Img & ": Server is unreachable. Trying again.", Logger.Warn);
          
          delay Duration(Wait_Between_Tries);
          
          if I = Tries then
            raise;
          end if;
        when others => raise;
      end;
    end loop;
    
    return ASU.To_String(Response);
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
    
    Logger.Put_Line(Command, Logger.Info, "-->");
    
    String'Output(
      S, 
      Crypto_Helper.Encrypt(Command, "Keeey")
    );
    
    declare
      Str : String := String'Input(S);
    begin
      Logger.Put_Line(Str, Logger.Info, "<--");
      Close_Selector(Read_Selector);
      Finalize;
      
      return Crypto_Helper.Decrypt(Str, "Keeeey");
    end;
  exception
    when Error : others =>
--      ShutDown_Socket(Sock);
      Close_Selector(Read_Selector);
      Finalize;
      
      raise;
  end Send;
  
  
  function String_To_Worker_Type(Arg : String) return Worker_Type is
  begin
    if Application_Helper.Is_Equal(Arg, "Master", true) then
      return Master;
    elsif Application_Helper.Is_Equal(Arg, "Mapper", true) then
      return Mapper;
    elsif Application_Helper.Is_Equal(Arg, "Reducer", true) then
      return Reducer;
    else
      raise Unknow_Worker_Type;
    end if;
  end String_To_Worker_Type;
  
  
  function To_String(Arg : Worker_Type) return String is
  begin
    case Arg is
      when Master => return "Master";
      when Mapper => return "Mapper";
      when Reducer => return "Reducer";
      when Invalid => return "N/A";
    end case;
  end To_String;
  
  
  function Trim(Input : String) return String is
  begin
    return Ada.Strings.Fixed.Trim(Input, Ada.Strings.Both);
  end Trim;
  
  
  procedure Parse_Configuration(Config_File : String; W_Type : Worker_Type) is
    Config_Xml : Xml.Node_Access;
  begin
    
    if Does_File_Exist(Config_File) then
      Logger.Put_Line("Parsing configuration file", Logger.Info);
      Config_Xml := Xml_Parser.Parse(File_Name => Config_File);
    else
      Logger.Put_Line("No configuration file found", Logger.Err);
      raise Configuration_File_Error;
    end if;
    
    
  -- only for MAPPER or REDUCER
  ------------------------------
    if W_Type = Mapper or W_Type = Reducer then
      
      -- identifier
      String_String_Maps.Insert(
        Configuration,
        "identifier",
        Xml.Get_Value(Config_Xml, "identifier")
      );
      
      
      
      -- master configutation
      declare
        Master_Details : Xml.Node_Access;
      begin
        Master_Details := Xml.Find_Child_With_Tag(Config_Xml, "master");
        
        Application_Helper.String_String_Maps.Insert(
          Application_Helper.Configuration,
          "master-ip",
          Xml.Get_Value(Master_Details, "ip")
        );

        Application_Helper.String_String_Maps.Insert(
          Application_Helper.Configuration,
          "master-port",
          Xml.Get_Value(Master_Details, "port")
        );
      end;
      
    end if;
    
    
    
  -- for ALL worker!
  ------------------------------
    -- local server configuration
    declare
      Local_Server_Details : Xml.Node_Access;
    begin
      Local_Server_Details := Xml.Find_Child_With_Tag(Config_Xml, "local_server");
      
      Add_Configuration("local_server", "bind_ip", Xml.Get_Value(Local_Server_Details, "bind_ip"));
      Add_Configuration("local_server", "bind_port", Xml.Get_Value(Local_Server_Details, "bind_port"));
    end;
    
    
    
    -- crypto configutation
    declare
      Crypto_Details : Xml.Node_Access;
      
      procedure Iterate_Crypto_Details(c: Xml.Node_Access_Vector.Cursor) is
        Node : Xml.Node_Access := Xml.Node_Access_Vector.Element(c);
      begin
        Add_Configuration("crypto", ASU.To_String(Node.Tag), ASU.To_String(Node.Value));
      end Iterate_Crypto_Details;
      
    begin
      Crypto_Details := Xml.Find_Child_With_Tag(Config_Xml, "crypto");
      Xml.Node_Access_Vector.Iterate(Crypto_Details.Children, Iterate_Crypto_Details'Access);
    end;
    
    
    
    -- settings
    declare
      Settings_Details : Xml.Node_Access;
      
      procedure Iterate_Settings_Details(c: Xml.Node_Access_Vector.Cursor) is
        Node : Xml.Node_Access := Xml.Node_Access_Vector.Element(c);
      begin
        Add_Configuration("settings", ASU.To_String(Node.Tag), ASU.To_String(Node.Value));
      end Iterate_Settings_Details;
      
    begin
      Settings_Details := Xml.Find_Child_With_Tag(Config_Xml, "settings");
      Xml.Node_Access_Vector.Iterate(Settings_Details.Children, Iterate_Settings_Details'Access);
    end;
    
    
    
    -- user settings
    declare
      User_Details : Xml.Node_Access;
      
      procedure Iterate_User_Details(c: Xml.Node_Access_Vector.Cursor) is
        Node : Xml.Node_Access := Xml.Node_Access_Vector.Element(c);
      begin
        Add_Configuration("user", ASU.To_String(Node.Tag), ASU.To_String(Node.Value));
      end Iterate_User_Details;
      
    begin
      User_Details := Xml.Find_Child_With_Tag(Config_Xml, "user");
      Xml.Node_Access_Vector.Iterate(User_Details.Children, Iterate_User_Details'Access);
    exception
      when Error : Constraint_Error =>
        Logger.Put_Line("No user settings found", Logger.Warn);
    end;
    
    Logger.Put_Line("Configuration file successfully parsed", Logger.Info);
  exception
    when Error : others => 
      Application_Helper.Print_Exception(Error);
      Ada.Exceptions.Raise_Exception(Configuration_File_Error'Identity, "There is a problem with the configuration file.");
  end;
  
  
  
  procedure Print_Configuration is
    
    procedure Print_Entry(C : String_String_Maps.Cursor) is
    begin
      Put(
        To_Upper(String_String_Maps.Key(C)),
        40, 2
      );
      
      Put_Line(
        String_String_Maps.Element(C),
        60, 2
      );
    end Print_Entry;
    
  begin
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("**************************************");
    Ada.Text_IO.Put_Line("Configuration parameter");
    Ada.Text_IO.Put_Line("--------------------------------------");
    String_String_Maps.Iterate(Configuration, Print_Entry'Access);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
  end Print_Configuration;
  
  
  function Read_Configuration(Prefix : String; Key : String) return String is
  begin
    return Read_Configuration(To_Lower(Prefix & "-" & Key));
  end Read_Configuration;
  
  
  function Read_Configuration(Key : String) return String is
  begin
    return To_Lower(String_String_Maps.Element(
      String_String_Maps.Find(Configuration, To_Lower(Key))
    ));
  exception
      when Error : Constraint_Error =>
        Ada.Exceptions.Raise_Exception(Configuration_Param_Not_Found'Identity, "Key """ & Key & """ not found in the configuration");
  end Read_Configuration;
  
  
  procedure Set_Default_Configuration(W_Type : Worker_Type) is
  begin
    Add_Configuration("settings", "max_connection_tries", "5");
    Add_Configuration("settings", "timeout_connection_tries", "5");
  end Set_Default_Configuration;
  
  
  procedure Add_Configuration(Prefix : String; Key : String; Value : String) is
  begin
    Add_Configuration(Prefix & "-" & Key, Value);
  end Add_Configuration;
  
  
  procedure Add_Configuration(Key : String; Value : String) is
    Lower_Key : String := To_Lower(Key);
    C         : String_String_Maps.Cursor;
  begin
    C := String_String_Maps.Find(Configuration, Lower_Key);
    
    if String_String_Maps."="(C, String_String_Maps.No_Element) then
      String_String_Maps.Insert(
        Configuration,
        Lower_Key,
        Value
      );
    else
      String_String_Maps.Replace(
        Configuration,
        Lower_Key,
        Value
      );
    end if;
  end Add_Configuration;
  
  
end Application_Helper;