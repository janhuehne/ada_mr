with Ada.Text_IO;

package body Rc_4 is
  
  --Random keygenerator based on Mapper_ID as initial value
  function Random_Key(In_Key : in Integer) return Key_Type is
      G : Random_Unsigned_Byte.Generator;
      Key : Key_Type;
  begin
      Random_Unsigned_Byte.Reset(G, In_Key);
      for I in 0..Key_Length-1 loop
          Key(I) := Random_Unsigned_Byte.Random(G);
      end loop;
      return Key;
  end Random_Key;
  
  
  --Initiating RC4 with a key, returning state of S-Box 
  procedure Init_RC4(Key : in Key_Type; UB_Array : out Unsigned_Byte_Array) is
      TMP_Unsigned_Char : Unsigned_Byte;
      J : Integer := 0;
  begin
      for I in 0..255 loop
          UB_Array(I) := Unsigned_Byte(I);
      end loop;
      
      for I in 0..255 loop
          J := (J + Integer(Key(I mod Key_Length)) +Integer(UB_Array(I))) mod 256;
          
          TMP_Unsigned_Char := UB_Array(I);
          UB_Array(I) := UB_Array(J);
          UB_Array(J) := TMP_Unsigned_Char;
      end loop;
  end Init_RC4;
  
  
  -- Encrypts / decrypts a byte
  procedure Encrypt(UB_Array : in out Unsigned_Byte_Array; Value : in out Unsigned_Byte; I : in out Natural; J : in out Natural) is
    Tmp : Unsigned_Byte;
    Random : Unsigned_Byte;
  begin
    I := (I + 1) mod UB_Array'Length;
    J := (J + Integer(UB_Array(J))) mod UB_Array'Length;
    
    --vertausche(s[i],s[j])
    Tmp := UB_Array(I);
    UB_Array(I) := UB_Array(J);
    UB_Array(J) := Tmp;
    
    Random := UB_Array((Integer((UB_Array(I) + UB_Array(J))) mod UB_Array'Length));
    
    Value := Random XOR Value;
  end Encrypt;
  
  
  -- Encrypts / decrypts a byte array
  procedure Encrypt(UB_Array : in out Unsigned_Byte_Array; Input : in out Unsigned_Byte_Array) is
    X, Y : Natural := 0;
  begin
    for I in Input'Range loop
      Encrypt(UB_Array, Input(I), X, Y);
    end loop;
  end Encrypt;
  
  
  --Prints a given Key
  procedure Print_Key(Key : in Key_Type) is
  begin
      Ada.Text_IO.Put("Key: ");
      for I in 0..Key_Length-1 loop
          Ada.Text_IO.Put(Unsigned_Byte'Image(Key(I)));
      end loop;
      Ada.Text_IO.New_Line;
  end Print_Key;
  
  
  -- Prints a unsigned byte array
  procedure Print(Input : in Unsigned_Byte_Array) is
  begin
    Ada.Text_IO.Put("Array: ");
    for I in Input'First..Input'Last loop
        Ada.Text_IO.Put(Unsigned_Byte'Image(Input(I)));
    end loop;
    Ada.Text_IO.New_Line;
  end Print;
  
end Rc_4;