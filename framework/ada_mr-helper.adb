with Ada_Mr.Logger;
with Ada.Strings.Fixed;
with Ada_Mr.Crypt.Helper;
with Ada_Mr.Xml.Parser;
with Ada.Command_Line;

package body Ada_Mr.Helper is
  ---------------------------
  -- String_To_Worker_Type --
  ---------------------------
  function String_To_Worker_Type
    (Arg : String)
    return Worker_Type 
  is
  begin
    if Ada_Mr.Helper.Is_Equal(Arg, "Master", true) then
      return Master;
    elsif Ada_Mr.Helper.Is_Equal(Arg, "Mapper", true) then
      return Mapper;
    elsif Ada_Mr.Helper.Is_Equal(Arg, "Reducer", true) then
      return Reducer;
    else
      raise Unknow_Worker_Type;
    end if;
  end String_To_Worker_Type;
  
  
  
  ---------------
  -- To_String --
  ---------------
  function To_String
    (Arg : Worker_Type)
    return String
  is
  begin
    case Arg is
      when Master => return "Master";
      when Mapper => return "Mapper";
      when Reducer => return "Reducer";
      when Invalid => return "N/A";
    end case;
  end To_String;
  
  
  
  -----------------
  -- Starts_With --
  -----------------
  function Starts_With
    (Item        : String;
     Pattern     : String;
     Ignore_Case : Boolean := false)
    return Boolean 
  is
  begin
    return Is_Equal(Item(Pattern'First .. Pattern'Last), Pattern, Ignore_Case);
  exception
    when others => return false;
  end Starts_With;
  
  
  
  --------------
  -- Is_Equal --
  --------------
  function Is_Equal
    (Arg_1       : String;
     Arg_2       : String;
     Ignore_Case : Boolean := false)
    return Boolean 
  is
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
  
  
  function Is_Equal
    (Item         : String;
     Input_Length : Natural;
     Pattern      : String;
     Ignore_Case  : Boolean := false)
    return Boolean 
  is
  begin
    return Is_Equal(Item(Item'First .. Input_Length), Pattern, Ignore_Case);
  exception
    when others => return false;
  end Is_Equal;
  
  
  function Is_Equal
    (Arg_1       : String;
     Arg_2       : Ada.Strings.Unbounded.Unbounded_String;
     Ignore_Case : Boolean := false) 
    return Boolean
  is
  begin
    return Is_Equal(Arg_1, Ada.Strings.Unbounded.To_String(Arg_2), Ignore_Case);
  exception
    when others => return false;
  end Is_Equal;
  
  
  
  -------------
  -- Sub_Str --
  -------------
  function Sub_Str
    (Input : String;
     From  : Integer;
     To    : Integer)
    return String 
  is
    Left  : Integer := 0;
    Right : Integer := 0;
  begin
    if Input'First <= From then
      Left := From;
    else
      Left := Input'First;
    end if;
      
    if Input'Last >= To then
      Right := To;
    else
      Right := Input'Last;
    end if;
    
    return Input(Left .. Right);
  end Sub_Str;
  
  
  
  ---------
  -- Put --
  ---------
  procedure Put
    (Str          : String;
     Field_Length : Natural := 0;
     Space_Pos    : Natural := 1) 
  is
    Field : Natural := Field_Length;
    
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
  
  
  
  --------------
  -- Put_Line --
  --------------
  procedure Put_Line
    (Str          : String;
     Field_Length : Natural := 0;
     Space_Pos    : Natural := 1)
  is
  begin
    Put(Str, Field_Length, Space_Pos);
    Ada.Text_IO.New_Line;
  end Put_Line;
  
  
  
  ---------------------
  -- Does_File_Exist --
  ---------------------
  function Does_File_Exist
    (Name : String) 
    return Boolean
  is
    The_File : Ada.Text_IO.File_Type;
  begin
    Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Name);
    Ada.Text_IO.Close (The_File);
    return True;
  exception
    when Ada.Text_IO.Name_Error => return False;
  end Does_File_Exist;
  
  
  
  ---------------------
  -- Print_Exception --
  ---------------------
  procedure Print_Exception
    (Error  : Ada.Exceptions.Exception_Occurrence; 
    Message : String := "")
  is
  begin
    if Message /= "" then
      Ada_Mr.Logger.Put_Line(Message, Ada_Mr.Logger.Info);
      Ada_Mr.Logger.Put_Line("-----------------------------------------------", Ada_Mr.Logger.Info);
    end if;
    
    Ada_Mr.Logger.Put_Line(Ada.Exceptions.Exception_Message(Error) & " (" & Ada.Exceptions.Exception_Name(Error) & ")", Ada_Mr.Logger.Err);
  end Print_Exception;
  
  
  
  ----------
  -- Send --
  ----------
  function Send
    (Host               : String;
     Port               : String;
     Command            : String; 
     Tries              : Natural := 1; 
     Wait_Between_Tries : Natural := 5)
  return String
  is
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
  
  
  function Send
    (Host               : String;
     Port               : GNAT.Sockets.Port_Type;
     Command            : String;
     Tries              : Natural := 1;
     Wait_Between_Tries : Natural := 5)
    return String 
  is
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
  
  
  function Send
    (Host               : GNAT.Sockets.Inet_Addr_Type;
     Port               : GNAT.Sockets.Port_Type;
     Command            : String;
     Tries              : Natural := 1;
     Wait_Between_Tries : Natural := 5)
    return String
  is
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
  
  
  function Send
    (Addr               : GNAT.Sockets.Sock_Addr_Type;
     Command            : String;
     Tries              : Natural;
     Wait_Between_Tries : Natural := 5)
    return String
  is
    Response : ASU.Unbounded_String;
  begin
    for I in 1 .. Tries loop
      declare
      begin
        Response := ASU.To_Unbounded_String(Send(Addr, Command));
        exit;
      exception
        when GNAT.Sockets.Socket_Error =>
          Ada_Mr.Logger.Put_Line("Attempt " & I'Img & ": Server is unreachable. Trying again.", Ada_Mr.Logger.Warn);
          
          delay Duration(Wait_Between_Tries);
          
          if I = Tries then
            raise;
          end if;
        when others => raise;
      end;
    end loop;
    
    return ASU.To_String(Response);
  end Send;
  
  
  function Send
    (Addr : GNAT.Sockets.Sock_Addr_Type;
     Command : String)
    return String 
  is
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
    
    Ada_Mr.Logger.Put_Line(Command, Ada_Mr.Logger.Info, "-->");
    
    String'Output(
      S, 
      Ada_Mr.Crypt.Helper.Encrypt(Command, "Keeey")
    );
    
    declare
      Str : String := String'Input(S);
    begin
      Ada_Mr.Logger.Put_Line(Str, Ada_Mr.Logger.Info, "<--");
      Close_Selector(Read_Selector);
      Finalize;
      
      return Ada_Mr.Crypt.Helper.Decrypt(Str, "Keeeey");
    end;
  exception
    when Error : others =>
      Close_Selector(Read_Selector);
      Finalize;
      
      raise;
  end Send;
  
  
  
  ----------
  -- Trim --
  ----------
  function Trim
    (Input : String)
    return String
  is
  begin
    return Ada.Strings.Fixed.Trim(Input, Ada.Strings.Both);
  end Trim;
  
  
  
  -------------------------
  -- Parse_Configuration --
  -------------------------
  procedure Parse_Configuration
    (Config_File : String;
     W_Type : Worker_Type)
  is
    Config_Xml : Ada_Mr.Xml.Node_Access;
  begin
    
    if Does_File_Exist(Config_File) then
      Ada_Mr.Logger.Put_Line("Parsing configuration file", Ada_Mr.Logger.Info);
      Config_Xml := Ada_Mr.Xml.Parser.Parse(File_Name => Config_File);
    else
      raise Configuration_File_Not_Found;
    end if;
    
    
  -- only for MAPPER or REDUCER
  ------------------------------
    if W_Type = Mapper or W_Type = Reducer then
      
      -- identifier
      begin
        Add_Configuration("identifier",  Ada_Mr.Xml.Get_Value(Config_Xml, "identifier"));
      exception
        when Error : others => null;
      end;
      
      -- master configutation
      declare
        Master_Details : Ada_Mr.Xml.Node_Access;
      begin
        Master_Details := Ada_Mr.Xml.Find_Child_With_Tag(Config_Xml, "master");
        
        begin
          Add_Configuration("master", "ip", Ada_Mr.Xml.Get_Value(Master_Details, "ip"));
        exception
          when others => Ada_Mr.Logger.Put_Line("No master ip found. Using defaults.", Ada_Mr.Logger.Warn);
        end;
        
        begin
          Add_Configuration("master", "port", Ada_Mr.Xml.Get_Value(Master_Details, "port"));
        exception
          when others => Ada_Mr.Logger.Put_Line("No master port found. Using defaults.", Ada_Mr.Logger.Warn);
        end;
        
      exception
        when Error : Constraint_Error =>
          Ada_Mr.Logger.Put_Line("No master settings found. Using defaults.", Ada_Mr.Logger.Warn);
      end;
      
    end if;
    
    
    
  -- for ALL worker!
  ------------------------------
    -- local server configuration
    declare
      Local_Server_Details : Ada_Mr.Xml.Node_Access;
    begin
      Local_Server_Details := Ada_Mr.Xml.Find_Child_With_Tag(Config_Xml, "local_server");
      
      begin
        Add_Configuration("local_server", "ip", Ada_Mr.Xml.Get_Value(Local_Server_Details, "ip"));
      exception
        when others => Ada_Mr.Logger.Put_Line("No local server ip found. Using defaults.", Ada_Mr.Logger.Warn);
      end;
      
      begin
        Add_Configuration("local_server", "port", Ada_Mr.Xml.Get_Value(Local_Server_Details, "port"));
      exception
        when others => Ada_Mr.Logger.Put_Line("No local server port found. Using defaults.", Ada_Mr.Logger.Warn);
      end;
      
    exception
      when Error : Constraint_Error =>
        Ada_Mr.Logger.Put_Line("No local server settings found. Using defaults.", Ada_Mr.Logger.Warn);
    end;
    
    
    
    -- crypto configutation
    declare
      Crypto_Details : Ada_Mr.Xml.Node_Access;
      
      procedure Iterate_Crypto_Details(c: Ada_Mr.Xml.Node_Access_Vector.Cursor) is
        Node : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Node_Access_Vector.Element(c);
      begin
        Add_Configuration("crypto", ASU.To_String(Node.Tag), ASU.To_String(Node.Value));
      end Iterate_Crypto_Details;
      
    begin
      Crypto_Details := Ada_Mr.Xml.Find_Child_With_Tag(Config_Xml, "crypto");
      Ada_Mr.Xml.Node_Access_Vector.Iterate(Crypto_Details.Children, Iterate_Crypto_Details'Access);
    exception
      when Error : Constraint_Error =>
        Ada_Mr.Logger.Put_Line("No crypto settings found. Using defaults.", Ada_Mr.Logger.Warn);
    
    end;
    
    
    
    -- settings
    declare
      Settings_Details : Ada_Mr.Xml.Node_Access;
      
      procedure Iterate_Settings_Details(c: Ada_Mr.Xml.Node_Access_Vector.Cursor) is
        Node : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Node_Access_Vector.Element(c);
      begin
        Add_Configuration("settings", ASU.To_String(Node.Tag), ASU.To_String(Node.Value));
      end Iterate_Settings_Details;
      
    begin
      Settings_Details := Ada_Mr.Xml.Find_Child_With_Tag(Config_Xml, "settings");
      Ada_Mr.Xml.Node_Access_Vector.Iterate(Settings_Details.Children, Iterate_Settings_Details'Access);
    exception
      when Error : Constraint_Error =>
        Ada_Mr.Logger.Put_Line("No settings found. Using defaults.", Ada_Mr.Logger.Warn);
    end;
    
    
    
    -- user settings
    declare
      User_Details : Ada_Mr.Xml.Node_Access;
      
      procedure Iterate_User_Details(c: Ada_Mr.Xml.Node_Access_Vector.Cursor) is
        Node : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Node_Access_Vector.Element(c);
      begin
        Add_Configuration("user", ASU.To_String(Node.Tag), ASU.To_String(Node.Value));
      end Iterate_User_Details;
      
    begin
      User_Details := Ada_Mr.Xml.Find_Child_With_Tag(Config_Xml, "user");
      Ada_Mr.Xml.Node_Access_Vector.Iterate(User_Details.Children, Iterate_User_Details'Access);
    exception
      when Error : Constraint_Error =>
        Ada_Mr.Logger.Put_Line("No user settings found", Ada_Mr.Logger.Warn);
    end;
    
    Ada_Mr.Logger.Put_Line("Configuration file successfully parsed", Ada_Mr.Logger.Info);
  exception
    when Configuration_File_Not_Found => 
      Ada_Mr.Logger.Put_Line("Configuration file: """ & Config_File & """ not found!", Ada_Mr.Logger.Warn);
    
    when Error : others => 
      Ada.Exceptions.Raise_Exception(Configuration_File_Error'Identity, "There is a problem with the configuration file.");
  end;
  
  
  
  -------------------------
  -- Print_Configuration --
  -------------------------
  procedure Print_Configuration is
    
    procedure Print_Entry
      (C : String_String_Maps.Cursor)
    is
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
  
  
  
  ------------------------
  -- Read_Configuration --
  ------------------------
  function Read_Configuration
    (Prefix : String; 
     Key : String)
    return String 
  is
  begin
    return Read_Configuration(To_Lower(Prefix & "-" & Key));
  end Read_Configuration;
  
  
  function Read_Configuration
    (Key : String)
    return String
  is
  begin
    return String_String_Maps.Element(
      String_String_Maps.Find(Configuration, To_Lower(Key))
    );
  exception
      when Error : Constraint_Error =>
        Ada.Exceptions.Raise_Exception(Configuration_Param_Not_Found'Identity, "Key """ & Key & """ not found in the configuration");
  end Read_Configuration;
  
  
  
  --------------------------------
  -- Read_Configuration_Or_Null --
  --------------------------------
  function Read_Configuration_Or_Null
    (Key : String)
    return String
  is
  begin
    return Read_Configuration(Key);
  exception
    when Error : others => return "";
  end Read_Configuration_Or_Null;
  
  
  function Read_Configuration_Or_Null
    (Prefix : String;
     Key : String)
    return String
  is
  begin
    return Read_Configuration(Prefix & "-" & Key);
  exception
    when Error : others => return "";
  end Read_Configuration_Or_Null;
  
  
  
  -------------------------------
  -- Set_Default_Configuration --
  -------------------------------
  procedure Set_Default_Configuration
    (W_Type : Worker_Type)
  is
  begin
    -- default config file
    case W_Type is
      when Master  => Add_Configuration("config_file", "master_config.xml");
      when Mapper  => Add_Configuration("config_file", "mapper_config.xml");
      when Reducer => Add_Configuration("config_file", "reducer_config.xml");
      when others  => null;
    end case;
    
    
    -- default local server ip
    Add_Configuration("local_server", "ip", "0.0.0.0");
    
    
    -- default local server port
    case W_Type is
      when Master  => Add_Configuration("local_server", "port", "7000");
      when Mapper  => Add_Configuration("local_server", "port", "8000");
      when Reducer => Add_Configuration("local_server", "port", "9000");
      when others  => null;
    end case;
    
    
    -- default master
    if W_Type = Mapper or W_Type = Reducer then
      Add_Configuration("master", "ip", "127.0.0.1");
      Add_Configuration("master", "port", "7000");
    end if;
    
    
    -- default crypto settings
    Add_Configuration("crypto", "hmac", "Default_Not_Secure");
    
    
    -- default settings
    Add_Configuration("settings", "max_connection_tries", "5");
    Add_Configuration("settings", "timeout_connection_tries", "5");
    Add_Configuration("settings", "log_level", "err");
  end Set_Default_Configuration;
  
  
  
  -----------------------
  -- Add_Configuration --
  -----------------------
  procedure Add_Configuration
    (Prefix : String;
     Key : String;
     Value : String)
  is
  begin
    Add_Configuration(Prefix & "-" & Key, Value);
  end Add_Configuration;
  
  
  
  -----------------------
  -- Add_Configuration --
  -----------------------
  procedure Add_Configuration
    (Key : String;
     Value : String) 
  is
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
  
  
  
  ----------------------------------
  -- Parse_Command_Line_Arguments --
  ----------------------------------
  procedure Parse_Command_Line_Arguments
    (W_Type : Worker_Type) 
  is
    function Map_Command_Line_Argument_To_Config_Argument
      (Command_Line_Argument : String)
    return String
    is
    begin
      if Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "i") then
        return "identifier";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "ls-ip") then
        return "local_server-ip";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "ls-port") then
        return "local_server-port";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "m-ip") then
        return "master-ip";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "m-port") then
        return "master-port";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "hmac") then
        return "crypto-hmac";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "mct") then
        return "settings-mac_connection_tries";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "tct") then
        return "settings-timeout_connection_tries";
      elsif Ada_Mr.Helper.Is_Equal(Command_Line_Argument, "ll") then
        return "settings-log_level";
      else
        return Command_Line_Argument;
      end if;
    end Map_Command_Line_Argument_To_Config_Argument;
    
  begin
    -- Set default configuration
    Set_Default_Configuration(W_Type);
    
    
    -- check for configfile argument
    for I in 1..Ada.Command_Line.Argument_Count loop
      if Starts_With(Ada.Command_Line.Argument(I), "-config_file=", true) then
         Add_Configuration("config_file", Ada.Command_Line.Argument(I)(14 .. Ada.Command_Line.Argument(I)'Last));
      end if;
    end loop;
    
    
    -- parse config file
    begin
      Parse_Configuration(Read_Configuration("config_file"), W_Type);
    exception
      when Error : others => Ada_Mr.Helper.Print_Exception(Error);
    end;
    
    
    -- store command line argument
    for I in 1..Ada.Command_Line.Argument_Count loop
      declare 
        Argument : String := Ada.Command_Line.Argument(I);
      begin
        if Starts_With(Argument, "-", true) then
          
          declare
            Input : String := Argument(2 .. Argument'Last);

            Dividor : Natural := Ada.Strings.Fixed.Index(Argument, "=");
            Key     : String  := Input(Input'First .. Dividor-1);
            Value   : String  := Input(Dividor+1 .. Input'Last);
          begin
            if Starts_With(Key, "identifier", true) then
              if W_Type = Mapper or W_Type = Reducer then
                Add_Configuration(Map_Command_Line_Argument_To_Config_Argument(Key), Value);
              end if;
            else
              Add_Configuration(Map_Command_Line_Argument_To_Config_Argument(Key), Value);
            end if;
          end;
        end if;
      end;
    end loop;
    
    
    -- read log level
    begin
      Logger.Set_Output_Level(
        Read_Configuration("settings", "log_level")
      );
    exception
      when others => null;
    end;
  end Parse_Command_Line_Arguments;
  
  
  
end Ada_Mr.Helper;