with Ada.Numerics.Discrete_Random;

package Rc_4 is
  
  type Unsigned_Byte is mod 256;
--  type Unsigned_Byte_Array is array(0 .. 255) of Unsigned_Byte;
  type Unsigned_Byte_Array is array(Natural range <>) of Unsigned_Byte;
  subtype S_Box is Unsigned_Byte_Array(0 .. 255);
  
  Key_Length : constant Integer := 4; 
  subtype Key_Type is Unsigned_Byte_Array(0 .. Key_Length-1);
  
  package Random_Unsigned_Byte is new Ada.Numerics.Discrete_Random(Unsigned_Byte);
  
  
  -- Random keygenerator based on Mapper_ID as initial value
  function Random_Key(In_Key : in Integer) return Key_Type;
  
  -- Initiating RC4 with a key, returning state of S-Box 
  procedure Init_RC4(Key : in Key_Type; UB_Array : out Unsigned_Byte_Array);
  
  -- Encrypts / decrypts a byte
  procedure Encrypt(UB_Array : in out Unsigned_Byte_Array; Value : in out Unsigned_Byte; I : in out Natural; J : in out Natural);
  
  -- Encrypts / decrypts a byte array
  procedure Encrypt(UB_Array : in out Unsigned_Byte_Array; Input : in out Unsigned_Byte_Array);
  
  --Prints a given Key
  procedure Print_Key(Key : in Key_Type);
  
  -- Prints a unsigned byte array
  procedure Print(Input : in Unsigned_Byte_Array);
  
end Rc_4;