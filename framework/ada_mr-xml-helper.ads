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
--    Helper package for the Ada_Mr.Xml component. Provides a couple 
--    of helper methods.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada_Mr.Helper;
with Ada.Strings.Unbounded;
with Ada_Mr.Xml;
with GNAT.Sockets;
with Ada.Exceptions;

package Ada_Mr.Xml.Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Group_Tag is
    (Mapper,
     Reducer,
     Master);
  
  function To_String
    (G_T : Group_Tag)
    return String;
  -- Returns a group tag as a string.
  
  function Xml_Command
    (G_T     : Group_Tag;
     Command : String;
     Details : String := "")
    return String;
  -- Forms a xml string with the given attributes
  
  function Xml_Command
    (G_T : Group_Tag; 
     Command : String; 
     Access_Token : String; 
     Details : String := "") 
    return String;
  -- Forms a xml string with the given attributes
  
  function Xml_Command
    (G_T : Group_Tag;
     Command : String;
     Details : Ada_Mr.Helper.String_String_Maps.Map)
    return String;
  -- Forms a xml string with the given attributes
  
  function Xml_Command
    (G_T : Group_Tag;
     Command : String;
     Access_Token : String;
     Details : Ada_Mr.Helper.String_String_Maps.Map)
    return String;
  -- Forms a xml string with the given attributes
  
  
  function Create_Initialization
    (G_T        : Group_Tag; 
     Identifier : String; 
     Ip         : GNAT.Sockets.Inet_Addr_Type; 
     Port       : GNAT.Sockets.Port_Type) 
    return String;
  -- Returns the initialization xml string
  
  function Create_Job_Request
    return String;
  -- Returns the xml string for a job request.
  
  function Create_System_Control
    (G_T     : Group_Tag; 
     Message : String) 
    return String;
  -- Returns the xml string with a system status.
  
  function Hash_To_Xml_String
    (Details : Ada_Mr.Helper.String_String_Maps.Map) 
    return String;
  -- Serializes a hash map to a string.
  
  function Request_From
    (Node : Ada_Mr.Xml.Node_Access) 
    return Ada_Mr.Helper.Worker_Type;
  -- Returns the worker type from which the requst came.
  
  function Is_Command
    (Node    : Ada_Mr.Xml.Node_Access; 
     Command : String) return Boolean;
  -- Returns <code>True</code> if the command tag in the xml tress is equal
  -- to <code>Command</code>.
  
  procedure Send_Error
    (S     : GNAT.Sockets.Stream_Access; 
     G_T   : Group_Tag; 
     Error : Ada.Exceptions.Exception_Occurrence);
  -- Sends the exception <code>Error</code> to a Stream <code>S</code>.
  
  function Get_Verified_Content
    (Xml_Root : Ada_Mr.Xml.Node_Access) 
    return Ada_Mr.Xml.Node_Access;
  -- Returns the verfified (by hmac) content.
  
end Ada_Mr.Xml.Helper;