-------------------------------------------------------------------------------
-- <STRONG>Copyright &copy; 2009, 2010 by Jan-Hendrik H&uuml;hne.</STRONG>
-- <BLOCKQUOTE>
--   This program is free software; you can redistribute it and/or
--   modify it under the terms of the GNU General Public License as
--   published by the Free Software Foundation; either version 2 of the
--   License, or (at your option) any later version.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--   General Public License for more details.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   You should have received a copy of the GNU General Public License
--   along with this program; if not, write to the Free Software
--   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
--   02111-1307, USA.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   As a special exception, if other files instantiate generics from
--   this unit, or you link this unit with other files to produce an
--   executable, this unit does not by itself cause the resulting
--   executable to be covered by the GNU General Public License. This
--   exception does not however invalidate any other reasons why the
--   executable file might be covered by the GNU Public License.
-- </BLOCKQUOTE>
--
--  <AUTHOR>
--    Bauhaus-University Weimar<br />
--    Jan-Hendrik H&uuml;hne <jan.huehne@uni-weimar.de>
--  </AUTHOR>
--
--  <PURPOSE>
--    Helper package for the Ada_Mr.Crypto component. Provides a couple 
--    of helper methods.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Crypto.Types;
with Crypto.Symmetric.Mac.Hmac_SHA256;
with Ada.Strings.Unbounded;

package Ada_Mr.Crypt.Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Compute_HMAC 
    (Message : String; 
     Key     : String) return String;
  -- Returns the hmac as a string by a given message and a key.
  
  function Create_Access_Token
    (Identifier  : String; 
     Worker_Type : String) return String;
  -- Returns a MD5 hash as a string. This function computes 
  -- the access_token.
  
  function Encrypt
    (Message : String; 
     Key     : String) return String;
  -- Returns an encrypted message.
  
  function Decrypt
    (Message : String; 
     Key :     String) return String;
  -- Decrypts a message and returns it.
  
  Wrong_HMAC : exception;
  
end Ada_Mr.Crypt.Helper;