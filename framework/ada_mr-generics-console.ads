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
--    Generic package, that provides a console to handle user inputs.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

generic
  type To_Controll_Task_Access is private;
  with function Banner 
    return String;
  with procedure Process_User_Input
    (User_Input  : String; 
     To_Controll : To_Controll_Task_Access);
    
package Ada_Mr.Generics.Console is
  
  package ASU renames Ada.Strings.Unbounded;
  
  task type Console is
    entry Start
      (M_Arg : To_Controll_Task_Access);
  end Console;
  
end Ada_Mr.Generics.Console;