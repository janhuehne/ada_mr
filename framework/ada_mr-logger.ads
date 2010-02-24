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
--    A logger system.
--  </PURPOSE>
-------------------------------------------------------------------------------

package Ada_Mr.Logger is
  
  type Log_Level is 
    (Info, 
     Warn, 
     Err, 
     User, 
     System, 
     Debug);
  
  procedure Set_Output_Level
    (Level : Log_Level);
  -- Sets the output level
  
  procedure Set_Output_Level
    (Level : String);
  -- Sets the output level
  
  procedure Put
    (Item   : String;
     Level  : Log_Level;
     Prefix : String := "");
  -- Puts the <code>Item</code> on STD/IO with additions log informations.
  
  procedure Put_Line
    (Item   : String;
     Level  : Log_Level; 
     Prefix : String := "");
  -- Puts the <code>Item</code> on STD/IO with additions log informations.
  
  procedure New_Line
    (Level : Log_Level);
  -- Puts a new line on STD/IO with the log level.
  
  procedure Info
    (Item : String;
     Prefix : String := "");
  -- Wrapper for <code>Put_Line(Item, Info, Prefix);</code>.
  
  procedure Warn
    (Item   : String;
     Prefix : String := "");
  -- Wrapper for <code>Put_Line(Item, Warn, Prefix);</code>.
  
  procedure Error
    (Item   : String;
     Prefix : String := "");
  -- Wrapper for <code>Put_Line(Item, Error, Prefix);</code>.
    
  procedure User
    (Item : String;
     Prefix : String := "");
  -- Wrapper for <code>Put_Line(Item, User, Prefix);</code>
    
  procedure System
    (Item : String;
     Prefix : String := "");
  -- Wrapper for <code>Put_Line(Item, System, Prefix);</code>
    
  procedure Debug
    (Item : String;
     Prefix : String := "");
  -- Wrapper for <code>Put_Line(Item, Debug, Prefix);</code>
  
  function Image(Level : Log_Level) return String;
  -- Returns the given log level as a string
  
  function From_String(Level : String) return Log_Level;
  -- Converts a string to the log level
  
private
  function Now return String;
  -- Returns the current time
  
  function Has_Correct_Level(Level : Log_Level) return Boolean;
  -- Returns <code>True</code> if an output should occur.
  
  Output_Level : Log_Level := Info;
end Ada_Mr.Logger;