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

with Crypto.Symmetric.Oneway_Blockcipher;
with Crypto.Hashfunction;
with Crypto.Random;

package Crypto.Symmetric.Mac is

   procedure Fill36 (Word_Array  : out  Words);
   procedure Fill36 (DWord_Array : out  DWords);

   procedure Fill5C (Word_Array   : out  Words);
   procedure Fill5C (DWord_Array  : out  DWords);

   procedure Copy(Source : in Words;  Dest : out Words);
   procedure Copy(Source : in DWords; Dest : out DWords);

   function M_B_Length(H : Words)  return Natural;
   function M_B_Length(H : DWords) return Natural;


   ---------------------------------------------------------------------------
   ------------------------------PRIVATE--------------------------------------
   ---------------------------------------------------------------------------

   pragma Inline(Fill36, Fill5C);

   pragma Optimize (Time);


end Crypto.Symmetric.Mac;
