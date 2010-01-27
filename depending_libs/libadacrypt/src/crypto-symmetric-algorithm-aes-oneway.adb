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


package body Crypto.Symmetric.Algorithm.Aes.Oneway is

   ---------------------------------------------------------------------------
   -------------------------PREPARE_ONEWAY_KEY--------------------------------
   ---------------------------------------------------------------------------


   procedure Prepare_Oneway_Key128(Key       : in B_Block128;
                                   Cipherkey : out Cipherkey_Oneway_AES128) is
   begin
      Build_Encrypt_Key(Key, Words(Cipherkey), Nk=> Nk128);
   end Prepare_Oneway_Key128;

   procedure Prepare_Oneway_Key192(Key       : in B_Block192;
                                   Cipherkey : out Cipherkey_Oneway_AES192) is
   begin
      Build_Encrypt_Key(Key, Words(Cipherkey), Nk=> Nk192);
   end Prepare_Oneway_Key192;

   procedure Prepare_Oneway_Key256(Key       : in B_Block256;
                                   Cipherkey : out Cipherkey_Oneway_AES256) is
   begin
      Build_Encrypt_Key(Key, Words(Cipherkey), Nk=> Nk256);
   end Prepare_Oneway_Key256;


   ---------------------------------------------------------------------------
   --------------------------ENCRYPT_ONEWAY-----------------------------------
   ---------------------------------------------------------------------------

    procedure Encrypt_Oneway128(Cipherkey : in  Cipherkey_Oneway_AES128;
                               Plaintext  : in  B_Block128;
                               Ciphertext : out B_Block128) is
    begin
       Ciphertext := Encrypt(Words(Cipherkey), Plaintext);
    end Encrypt_Oneway128;

   procedure Encrypt_Oneway192(Cipherkey  : in  Cipherkey_Oneway_AES192;
                               Plaintext  : in  B_Block128;
                               Ciphertext : out B_Block128) is
   begin
      Ciphertext := Encrypt(Words(Cipherkey), Plaintext);
   end Encrypt_Oneway192;


   procedure Encrypt_Oneway256(Cipherkey  : in  Cipherkey_Oneway_AES256;
                               Plaintext  : in  B_Block128;
                               Ciphertext : out B_Block128) is
   begin
      Ciphertext := Encrypt(Words(Cipherkey), Plaintext);
   end Encrypt_Oneway256;


end Crypto.Symmetric.Algorithm.Aes.Oneway;
