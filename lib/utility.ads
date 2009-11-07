with Ada.Text_IO;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Ada.Strings.Unbounded;

with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash;

package Utility is
  function Starts_With(Item : String; Pattern : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Arg_1 : String; Arg_2 : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Item : String; Input_Length : Natural; Pattern : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Arg_1 : String; Arg_2 : Ada.Strings.Unbounded.Unbounded_String; Ignore_Case : Boolean := false) return Boolean;
  
  procedure Put(Str : String; Field_Length : Natural := 0; Space_Pos : Natural := 1);
  procedure Put_Line(Str : String; Field_Length : Natural := 0; Space_Pos : Natural := 1);
  
  function Does_File_Exist(Name : String) return Boolean;
  
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
end Utility;