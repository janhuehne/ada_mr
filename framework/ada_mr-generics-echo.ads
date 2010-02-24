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
--    Generic package, that provides a echo tasks, which is used for 
--    incomming server request. As well it provides a buffer to 
--    reactivate echo tasks.
--  </PURPOSE>
-------------------------------------------------------------------------------

with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Xml;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;

generic
  with procedure Process_Request
    (S        : Stream_Access;
     From     : Ada_Mr.Helper.Worker_Type;
     Xml_Root : Ada_Mr.Xml.Node_Access);
  
package Ada_Mr.Generics.Echo is
  
  package ASU renames Ada.Strings.Unbounded;
  
  Max_Tasks : CONSTANT Positive := 20;
  type Index is mod Max_Tasks;
  
  type Echo; 
  type Echo_Access is access Echo;
  
  task type Echo is
    entry Start
      (N_Sock : in Socket_Type; 
       Self   : in Echo_Access);
       
    entry ReStart
      (N_Sock : IN Socket_Type);
  end Echo;
  
  type Task_Array is array(Index) of Echo_Access;
  
  protected Buffer is
    entry Deposit
      (X : Echo_Access);
    -- Push a waiting echo task.
      
    entry Extract
      (X : out Echo_Access);
    -- Returns an waiting echo task and removes it.
      
    function Num_Waiting 
      return Natural;
    -- Returns the number of waiting echo tasks.
  private
     Buf : Task_Array;
     I, J : Index := 0;
     Count : Natural range 0 .. Max_Tasks := 0;
  end Buffer;
  
end Ada_Mr.Generics.Echo;