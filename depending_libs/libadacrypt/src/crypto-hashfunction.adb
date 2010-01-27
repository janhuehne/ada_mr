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


package body  Crypto.Hashfunction is

   H : Hash_Type;


   ---------------------------------------------------------------------------

   procedure Init is
   begin
      Init(H);
   end Init;

   ---------------------------------------------------------------------------

   procedure Round(Message_Block : in Message_Type) is
   begin
      Round(Message_Block, H);
   end Round;

   ---------------------------------------------------------------------------

   function Final_Round(Last_Message_Block  : Message_Type;
                        Last_Message_Length : Message_Block_Length_Type)
                        return Hash_Type is
   begin
      return Final_Round(Last_Message_Block, Last_Message_Length, H);
   end Final_Round;


   ---------------------------------------------------------------------------

   function Hash  (Message  : Bytes)  return Hash_Type is
   begin
      Hash(Message, H);
      return H;
   end Hash;

    ---------------------------------------------------------------------------

   function Hash  (Message  : String) return Hash_Type is
   begin
      Hash(Message, H);
      return H;
   end Hash;

   ---------------------------------------------------------------------------

   function F_Hash(Filename : String) return Hash_Type is
   begin
      F_Hash(Filename, H);
      return H;
   end F_Hash;

   end Crypto.Hashfunction;
