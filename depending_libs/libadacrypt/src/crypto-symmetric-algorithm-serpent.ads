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

-------------------------------------------------------------------------------
--
-- Serpent Blockcipher
--
-- Copyright (c) 1998 Markus G. Kuhn <mkuhn@acm.org>. All rights reserved.
--
-- $Id: crypto-symmetric-algorithm-serpent.ads 1.2 Fri, 17 Dec 2004 16:55:38 +0100 shortie $
--
-------------------------------------------------------------------------------
--
-- This is the Ada95 reference implementation of the Serpent cipher
-- submitted by Ross Anderson, Eli Biham and Lars Knudson in June 1998 to
-- the NIST Advanced Encryption Standard (AES) contest. Please note that
-- this is a revised algorithm that is not identical to the old version
-- presented at the 1998 Fast Software Encryption Workshop.
-- <http://www.cs.technion.ac.il/~biham/Reports/Serpent/>
--
-- Compiled with GNAT 3.10p under Linux, this implementation encrypts and
-- decrypts with 20.8 Mbit/s on a 300 MHz Pentium II.
--
-------------------------------------------------------------------------------

package Crypto.Symmetric.Algorithm.Serpent is


   type Cipherkey_Serpent256 is private;

   ---------------------------------------------------------------------------

   procedure Prepare_Key256(Key       : in  B_Block256;
                            Cipherkey : out Cipherkey_Serpent256);

   procedure Encrypt256(Cipherkey  : in  Cipherkey_Serpent256;
                        Plaintext  : in  B_Block128;
                        Ciphertext : out B_Block128);

   procedure Decrypt256(Cipherkey  : in  Cipherkey_Serpent256;
                        Ciphertext : in  B_Block128;
                        Plaintext  : out B_Block128);

   ---------------------------------------------------------------------------
   -----------------------------PRIVATE--------------------------------------
   ---------------------------------------------------------------------------


private

   subtype  Roundkey_Serpent256 is Words(-8 .. 131);

   type Cipherkey_Serpent256 is new Roundkey_Serpent256;

   pragma Inline (Prepare_Key256, Encrypt256, Decrypt256);
   pragma Optimize (Time);

   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------

end Crypto.Symmetric.Algorithm.Serpent;
