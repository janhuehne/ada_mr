with Ada.Text_IO;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Ada.Strings.Unbounded;


package Utility is
  function Starts_With(Item : String; Pattern : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Arg_1 : String; Arg_2 : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Item : String; Input_Length : Natural; Pattern : String; Ignore_Case : Boolean := false) return Boolean;
  function Is_Equal(Arg_1 : String; Arg_2 : Ada.Strings.Unbounded.Unbounded_String; Ignore_Case : Boolean := false) return Boolean;
end Utility;