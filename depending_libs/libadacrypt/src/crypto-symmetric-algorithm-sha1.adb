-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

-- This Implementation of SHA-1 uses the alternative method
-- inclusive loop unrolling (optimization)
-- For further information read FIPS 180-2

with Crypto.Symmetric.Algorithm.SHA_Utils;
use Crypto.Symmetric.Algorithm.SHA_Utils;

package body Crypto.Symmetric.Algorithm.SHA1 is

   Current_Message_Length : Message_Length64;

   function Ch( X,Y,Z : Word) return Word is
   begin
      return ((X  and Y) xor ((not X) and Z));
   end Ch; pragma Inline (Ch);

   ---------------------------------------------------------------------------

   function Parity(X,Y,Z : Word) return Word is
   begin
      return (X xor Y xor Z);
   end Parity; pragma Inline (Parity);

   ---------------------------------------------------------------------------

   function May(X,Y,Z : Word) return Word is
   begin
      return ((X and Y) xor (X and Z) xor (Y and Z));
   end May; pragma Inline (May);

   ---------------------------------------------------------------------------

   procedure Init(Hash_Value : out W_Block160) is
   begin
      Current_Message_Length:=0;
      Hash_Value(0):=16#67452301#;
      Hash_Value(1):=16#EfCdAb89#;
      Hash_Value(2):=16#98BaDcFe#;
      Hash_Value(3):=16#10325476#;
      Hash_Value(4):=16#C3D2E1F0#;
   end Init;

   ---------------------------------------------------------------------------

   -- FIPS 180-2 page 17-18 + loop unrolling

   procedure Round(Message_Block   : in W_Block512;
                        Hash_Value : in out W_Block160) is
      SHA1_Constant1 : constant Word := 16#5a827999#;
      SHA1_Constant2 : constant Word := 16#6ed9eba1#;
      SHA1_Constant3 : constant Word := 16#8f1bbcdc#;
      SHA1_Constant4 : constant Word := 16#Ca62c1d6#;

      W : W_Block512:=Message_Block;
      MASK : constant WORD:=15;
      A, B, C, D, E, Z, S : Word;
   begin

      --0
      Current_Message_Length := Current_Message_Length + 512;

      if Current_Message_Length = 0  then
         raise SHA1_Constraint_Error;
      end if;

      --2
      A:=Hash_Value(0);
      B:=Hash_Value(1);
      C:=Hash_Value(2);
      D:=Hash_Value(3);
      E:=Hash_Value(4);

      --3
      for T in Word'First..15 loop
         S := T and MASK;
         Z:= Rotate_Left(A,5) + Ch(B,C,D) + E + SHA1_Constant1 + W(Integer(S));
         E:=D;
         D:=C;
         C:=Rotate_Left(B,30);
         B:=A;
         A:=Z;
      end loop;

      for T in Word(16)..19 loop
         S := T and MASK;
         W(Integer(S)):=Rotate_Left((W(Integer((S+13) and MASK)) xor
                                     W(Integer((S+8) and MASK)) xor
                                     W(Integer((S+2) and MASK)) xor
                                     W(Integer(S))),1);
         Z := Rotate_Left(A,5) + Ch(B,C,D) + E + SHA1_Constant1 +
           W(Integer(S));
         E:=D;
         D:=C;
         C:=Rotate_Left(B,30);
         B:=A;
         A:=Z;
      end loop;


      for T in Word(20)..39 loop
         S := T and MASK;
         W(Integer(S)):=Rotate_Left((W(Integer((S+13) and MASK)) xor
                                     W(Integer((S+8) and MASK)) xor
                                     W(Integer((S+2) and MASK)) xor
                                     W(Integer(S))),1);
         Z := Rotate_Left(A,5) + Parity(B,C,D) + E + SHA1_Constant2 +
           W(Integer(S));
         E:=D;
         D:=C;
         C:=Rotate_Left(B,30);
         B:=A;
         A:=Z;
      end loop;


      for T in Word(40)..59 loop
         S := T and MASK;
         W(Integer(S)):=Rotate_Left((W(Integer((S+13) and MASK)) xor
                                     W(Integer((S+8) and MASK)) xor
                                     W(Integer((S+2) and MASK)) xor
                                     W(Integer(S))),1);
         Z := Rotate_Left(A,5) + May(B,C,D) + E + SHA1_Constant3 +
           W(Integer(S));
         E:=D;
         D:=C;
         C:=Rotate_Left(B,30);
         B:=A;
         A:=Z;
      end loop;


      for T in Word(60)..79 loop
         S := T and MASK;
         W(Integer(S)):=Rotate_Left((W(Integer((S+13) and MASK)) xor
                                     W(Integer((S+8) and MASK)) xor
                                     W(Integer((S+2) and MASK)) xor
                                     W(Integer(S))),1);
         Z := Rotate_Left(A,5) + Parity(B,C,D) + E + SHA1_Constant4 +
           W(Integer(S));
         E:=D;
         D:=C;
         C:=Rotate_Left(B,30);
         B:=A;
         A:=Z;
      end loop;

      --4
      Hash_Value(0):=A+Hash_Value(0);
      Hash_Value(1):=B+Hash_Value(1);
      Hash_Value(2):=C+Hash_Value(2);
      Hash_Value(3):=D+Hash_Value(3);
      Hash_Value(4):=E+Hash_Value(4);
   end Round;

   ---------------------------------------------------------------------------

   function Final_Round(Last_Message_Block  : W_Block512;
                        Last_Message_Length : Message_Block_Length512;
                        Hash_Value          : W_Block160)
                       return W_Block160 is

      H  : W_Block160 := Hash_Value;
      MF : W_Block512 := Last_Message_Block;
      MP : W_Block512;
   begin
       -- If the last_message Block is a full block
      if Last_Message_Length = Message_Block_Length512'Last then
         Round(MF, H);
         MF := (others => 0);
      else
         Current_Message_Length := Current_Message_Length +
           Message_Length64(Last_Message_Length)*8;
      end if;

      Padding512(MF, Current_Message_Length, MP);
      Round(MF, H);

      if Is_Zero(MP) = False then
         Round(MP, H);
      end if;

      return H;

   end Final_Round;

   --------------------------------------------------------------------------

   procedure Hash(Message : in Bytes; Hash_Value : out W_Block160) is
      -- K == |Message| mod 512 == number of full message blocks
      K : constant Natural :=  Shift_Right(Message'Length,6);
      -- L = length of the last message
      L : constant Natural :=  Message'Length mod 64;
      LM : Natural;
      M : W_Block512 := (others=>0);

   begin
      Init(Hash_Value);

      for I in 1..K loop
         Round(To_Words(Message(1+Shift_left(I-1,6)..Shift_Left(I,6))),
               Hash_Value);
      end loop;

      if L /=  0 then
         LM := Shift_Right(L,2);
         if (L mod 4) = 0 then
            LM := LM-1;
         end if;

         M(M'First..LM) :=
           To_Words(Message(Message'Last-(L-1)..Message'Last));
      end if;
      Hash_Value :=  Final_Round(M, L, Hash_Value);
   end Hash;

   ---------------------------------------------------------------------------

   procedure Hash(Message : in String; Hash_Value : out W_Block160) is
   begin
      Hash(To_Bytes(Message), Hash_Value);
   end; pragma Inline (Hash);

   ---------------------------------------------------------------------------

   procedure F_Hash(Filename : String; Hash_Value : out W_Block160) is
      Size     : Integer;
      Buf      : Bytes(0..64);
      Fd       : File_Descriptor;
      B        : Byte_Word:=(others=>0);
      Bcounter : To_Word_Counter_Type:=0;
      Mcounter : Message_Counter_Type:=0;
      FilePath : constant string := Filename & ASCII.NUL;
      M        : W_Block512 := (others => 0); -- Messageblock
   begin
      Fd :=  Open_Read(FilePath'Address, Binary);
      if fd = invalid_fd then
         raise File_Open_Error;
      end if;

      Init(Hash_Value);

      loop
         Size := Read(Fd, Buf'Address , Buf'Last);
         if Size = Buf'Last then
            Round(To_Words(Buf(0..63)),Hash_Value);
         elsif
           Size < 0 then raise File_Read_Error;
         else
            for I in 0..Size-1 loop
               B(Integer(Bcounter)):= Buf(I);
                  Bcounter:=Bcounter+1;
                  if Bcounter = 0 then
                     M(Integer(Mcounter)):= To_Word(B);
                     B:=(others=>0);
                     Mcounter:= Mcounter+1;
                  end if;
            end loop;
            exit;
         end if;
      end loop;

      Close(Fd);

      if BCounter /= 0 then
         M(Integer(MCounter)):= To_Word(B);
      end if;

      Hash_Value :=
        Final_Round(M, Message_Block_Length512(Size), Hash_Value);
   end F_Hash;

   ---------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.SHA1;
