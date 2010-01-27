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


package body  Crypto.Symmetric.Algorithm.SHA1.Oneway is

   procedure Prepare_Key(Key       : in W_Block256;
                         Cipherkey : out Cipherkey_SHA1) is
      T1 : W_Block128:= Key(0..3);
      T2 : W_Block128:= Key(4..7);
      T3 : W_Block128:= Key(2..5);

      M : W_Block512; -- message block
      H : W_Block160; -- hash value
   begin
      Init(H);

      M( 0.. 3) := T1;
      M( 4.. 7) := T2;
      M( 8..11) := T3;
      M(12..15) := T1;
      Round(M, H);
      Cipherkey.Left_Key(5) := H(0);

      M( 0.. 3) := T2;
      M( 4.. 7) := T3;
      M( 8..11) := T1;
      M(12..15) := T2;
      Round(M, H);
      Cipherkey.Right_Key := H;

      M( 0.. 3) := T3;
      M( 4.. 7) := T1;
      M( 8..11) := T2;
      M(12..15) := T3;
      Round(M, H);
      Cipherkey.Left_Key(0..4) := H;

      M  := (others => 0);
      H  := (others => 0);
      T1 := (others => 0);
      T2 := (others => 0);
      T3 := (others => 0);

   end  Prepare_Key;

   ---------------------------------------------------------------------------

   procedure Encrypt(Cipherkey  : in  Cipherkey_SHA1;
                     Plaintext  : in  W_Block160;
                     Ciphertext : out W_Block160) is
      M : W_Block512;
   begin
      Init(Ciphertext);
      M( 0.. 5) := Cipherkey.Left_Key;
      M( 6..10) := Plaintext;
      M(11..15) := Cipherkey.Right_Key;
      Round(M, Ciphertext);
   end Encrypt;


end Crypto.Symmetric.Algorithm.SHA1.Oneway;


