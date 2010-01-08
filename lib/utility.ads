with Ada.Text_IO;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash;

with Ada.Exceptions;

with GNAT.Sockets;


package Utility is
  package ASU renames Ada.Strings.Unbounded;
  
  function Starts_With(Item : String; Pattern : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Arg_1 : String; Arg_2 : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Item : String; Input_Length : Natural; Pattern : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Arg_1 : String; Arg_2 : Ada.Strings.Unbounded.Unbounded_String; Ignore_Case : Boolean := false) return Boolean;
  
  function Trim(Input : String) return String;
  
  procedure Put(Str : String; Field_Length : Natural := 0; Space_Pos : Natural := 1);
  procedure Put_Line(Str : String; Field_Length : Natural := 0; Space_Pos : Natural := 1);
  
  function Does_File_Exist(Name : String) return Boolean;
  
  procedure Print_Exception(Error : Ada.Exceptions.Exception_Occurrence; Message : String := "");
  
  
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
  
  function Send(Host : String; Port : String; Command : String; Tries : Natural := 1; Wait_Between_Tries : Natural := 5) return String;
  function Send(Host : String; Port : GNAT.Sockets.Port_Type; Command : String; Tries : Natural := 1; Wait_Between_Tries : Natural := 5) return String;
  function Send(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type; Command : String; Tries : Natural := 1; Wait_Between_Tries : Natural := 5) return String;
  function Send(Addr : GNAT.Sockets.Sock_Addr_Type; Command : String; Tries : Natural; Wait_Between_Tries : Natural := 5) return String;
  function Send(Addr : GNAT.Sockets.Sock_Addr_Type; Command : String) return String;
  
  Compute_Job_Error : Exception;
  Unknown_Command : Exception;
  Initialisation_Failed : Exception;
  Configuration_File_Error : Exception;
  
  
  type Worker_Type is (
    Master,
    Mapper,
    Reducer,
    Invalid
  );
  
  function String_To_Worker_Type(Arg : String) return Worker_Type;
  function To_String(Arg : Worker_Type) return String;
  
  Unknow_Worker_Type : Exception;
  
end Utility;