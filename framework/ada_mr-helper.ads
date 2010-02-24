-------------------------------------------------------------------------------
-- <STRONG>Copyright &copy; 2009, 2010 by Jan-Hendrik H&uuml;hne.</STRONG>
-- <BLOCKQUOTE>
--   This program is free software; you can redistribute it and/or
--   modify it under the terms of the GNU General Public License as
--   published by the Free Software Foundation; either version 2 of the
--   License, or (at your option) any later version.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--   General Public License for more details.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   You should have received a copy of the GNU General Public License
--   along with this program; if not, write to the Free Software
--   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
--   02111-1307, USA.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   As a special exception, if other files instantiate generics from
--   this unit, or you link this unit with other files to produce an
--   executable, this unit does not by itself cause the resulting
--   executable to be covered by the GNU General Public License. This
--   exception does not however invalidate any other reasons why the
--   executable file might be covered by the GNU Public License.
-- </BLOCKQUOTE>
--
--  <AUTHOR>
--    Bauhaus-University Weimar<br />
--    Jan-Hendrik H&uuml;hne <jan.huehne@uni-weimar.de>
--  </AUTHOR>
--
--  <PURPOSE>
--    Application wide helper package. It provides several methods which 
--    can not assign to a specific component.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash;
with Ada.Exceptions;
with GNAT.Sockets;
with Ada_Mr.Xml;

package Ada_Mr.Helper is
  package ASU renames Ada.Strings.Unbounded;
  
  
  
  
  type Worker_Type is (
    Master,
    Mapper,
    Reducer,
    Invalid
  );

  function String_To_Worker_Type
    (Arg : String) 
    return Worker_Type;
  -- Converts a string into a worker type.
    
  function To_String
    (Arg : Worker_Type) 
    return String;
  -- Converts a worker type into a string.
  
  
  
  
  function Starts_With
    (Item        : String; 
     Pattern     : String; 
     Ignore_Case : Boolean := false) 
    return Boolean;
  -- Returns <code>True</code> if <code>Item</code> starts with <code>Pattern</code>. 
  -- If <code>Ignore_Case</code> is <code>True</code> the check is case sensitive.
  
  function Is_Equal
    (Arg_1       : String; 
     Arg_2       : String; 
     Ignore_Case : Boolean := false) 
    return Boolean;
  -- Returns <code>True</code> if <code>Arg_1</code> is euqal to <code>Arg_2</code>.
  -- If <code>Ignore_Case</code> is <code>True</code> the check is case sensitive.
  
  function Is_Equal
    (Item         : String; 
     Input_Length : Natural; 
     Pattern      : String; 
     Ignore_Case  : Boolean := false) 
    return Boolean;
  -- Returns <code>Is_Equal(Item(Item'First .. Input_Length), Pattern, Ignore_Case)</code>.
  -- If <code>Ignore_Case</code> is <code>True</code> the check is case sensitive.
  
  function Is_Equal
    (Arg_1       : String; 
     Arg_2       : Ada.Strings.Unbounded.Unbounded_String; 
     Ignore_Case : Boolean := false) 
    return Boolean;
  -- Returns <code>True</code> if <code>Arg_1</code> is euqal to <code>Arg_2</code>.
  -- If <code>Ignore_Case</code> is <code>True</code> the check is case sensitive.
  
  function Trim
    (Input : String) 
    return String;
  -- Returns the given string <code>Input</code> without leading or ending spaces.
  
  function Sub_Str
    (Input : String; 
     From  : Integer; 
     To    : Integer) 
    return String;
  -- Returns the given string <code>Input</code> from the position <code>From</code> to
  -- <code>To</code>
  
  procedure Put
    (Str          : String; 
     Field_Length : Natural := 0; 
     Space_Pos    : Natural := 1);
  -- Puts the given string <code>Str</code> on STD/IO. If <code>Str'Length < Field_Length<code>
  -- the remaining positions filled with spaces. If <code>Space_Pos = 0</code> its filled before
  -- <code>Str</code>, elsif <code>Space_Pos = 1</code>its filled behind.
  
  procedure Put_Line
    (Str : String; 
     Field_Length : Natural := 0; 
     Space_Pos : Natural := 1);
  -- Puts the given string <code>Str</code> on STD/IO. If <code>Str'Length < Field_Length<code>
  -- the remaining positions filled with spaces. If <code>Space_Pos = 0</code> its filled before
  -- <code>Str</code>, elsif <code>Space_Pos = 1</code>its filled behind.
  
  function Does_File_Exist
    (Name : String)
    return Boolean;
  -- Returns <code>True</code> if file in <code>Name</code> exists.
  
  procedure Print_Exception
    (Error  : Ada.Exceptions.Exception_Occurrence; 
    Message : String := "");
  -- Prints a expception on STD/IO.
  
  procedure Parse_Configuration
    (Config_File : String; 
     W_Type : Worker_Type);
  -- Parses a configuration file for a specified worker.
  
  procedure Print_Configuration;
  -- Prints the configuration on STD/IO.
  
  procedure Set_Default_Configuration
    (W_Type : Worker_Type);
  -- Sets the default configuration entries.
  
  function Read_Configuration
    (Key : String)
    return String;
  -- Looks for <code>key</code> in the configuration and returns it. If <code>key</code>
  -- can not found it raises the exception <code>Configuration_Param_Not_Found</code>.
  
  function Read_Configuration
    (Prefix : String;
     Key : String)
    return String;
  -- Looks for <code>prefix-key</code> in the configuration and returns it. If <code>prefix-key</code>
  -- can not found it raises the exception <code>Configuration_Param_Not_Found</code>.
  
  function Read_Configuration_Or_Null
    (Key : String)
    return String;
  -- Looks for <code>key</code> in the configuration and returns it. If <code>key</code>
  -- can not found it returns <code>""</code>.
  
  function Read_Configuration_Or_Null
    (Prefix : String;
     Key : String)
    return String;
  -- Looks for <code>prefix-key</code> in the configuration and returns it. If <code>prefix-key</code>
  -- can not found it returns <code>""</code>.
  
  procedure Add_Configuration
    (Key : String; 
     Value : String);
  -- Adds a new configuration entry.
  
  procedure Add_Configuration
    (Prefix : String; 
     Key : String; 
     Value : String);
  -- Adds a new configuration entry.
  
  procedure Parse_Command_Line_Arguments(W_Type : Worker_Type);
  -- Parses the command line arguments.
  
  
  
  
  package String_String_Maps is new Ada.Containers.Indefinite_Hashed_Maps(
    Key_Type        => String,
    Element_Type    => String,
    Hash            => Ada.Strings.Hash,
    Equivalent_Keys => "="
  );
  
  package String_Integer_Maps is new Ada.Containers.Indefinite_Hashed_Maps(
    Key_Type        => String,
    Element_Type    => Integer,
    Hash            => Ada.Strings.Hash,
    Equivalent_Keys => "="
  );
  
  
  
  
  function Send
    (Host               : String; 
     Port               : String; 
     Command            : String; 
     Tries              : Natural := 1; 
     Wait_Between_Tries : Natural := 5)
    return String;
  -- Sends the <code>Command</code> to the specified host in <code>Host</code> 
  -- and <code>Port</code>. If the sending failed, it will repeated <code>Tries</code>-times
  -- with a delay from <code>Wait_Between_Tries</code>. It returns the answer.
  
  function Send
    (Host               : String; 
     Port               : GNAT.Sockets.Port_Type; 
     Command            : String; 
     Tries              : Natural := 1; 
     Wait_Between_Tries : Natural := 5)
    return String;
  -- Sends the <code>Command</code> to the specified host in <code>Host</code> 
  -- and <code>Port</code>. If the sending failed, it will repeated <code>Tries</code>-times
  -- with a delay from <code>Wait_Between_Tries</code>. It returns the answer.
  
  function Send
    (Host               : GNAT.Sockets.Inet_Addr_Type; 
     Port               : GNAT.Sockets.Port_Type; 
     Command            : String; 
     Tries              : Natural := 1; 
     Wait_Between_Tries : Natural := 5)
    return String;
  -- Sends the <code>Command</code> to the specified host in <code>Host</code> 
  -- and <code>Port</code>. If the sending failed, it will repeated <code>Tries</code>-times
  -- with a delay from <code>Wait_Between_Tries</code>. It returns the answer.
  
  function Send
    (Addr               : GNAT.Sockets.Sock_Addr_Type; 
     Command            : String; 
     Tries              : Natural;
     Wait_Between_Tries : Natural := 5)
    return String;
  -- Sends the <code>Command</code> to the specified host in <code>Addr</code>.
  -- If the sending failed, it will repeated <code>Tries</code>-times with a 
  -- delay from <code>Wait_Between_Tries</code>. It returns the answer.
  
  function Send
    (Addr    : GNAT.Sockets.Sock_Addr_Type; 
     Command : String) 
    return String;
  -- Sends the <code>Command</code> to the specified host in <code>Addr</code>.
  -- It returns the answer.
  
  
  
  Compute_Job_Error : Exception;
  Unknown_Command : Exception;
  Initialisation_Failed : Exception;
  Configuration_File_Error : Exception;
  Configuration_File_Not_Found : Exception;
  Unknow_Worker_Type : Exception;
  Configuration_Param_Not_Found : Exception;

private
  Configuration : String_String_Maps.Map;
  
end Ada_Mr.Helper;