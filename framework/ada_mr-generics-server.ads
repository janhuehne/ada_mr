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
--    Generic package, that provides a server task. It accepts incomming
--    connections and passes them through to an echo task.
--  </PURPOSE>
-------------------------------------------------------------------------------

with GNAT.Sockets;
with Ada_Mr.Generics.Echo;
with Ada_Mr.Helper;
with Ada_Mr.Xml;

generic
   with function Exit_Server 
    return Boolean;
   with procedure Process_Request
     (S        : GNAT.Sockets.Stream_Access; 
      From     : Ada_Mr.Helper.Worker_Type; 
      Xml_Root : Ada_Mr.Xml.Node_Access);
     
package Ada_Mr.Generics.Server is
  
  package Echo is new Ada_Mr.Generics.Echo(Process_Request);
  
  task type Server_Task is
    entry Start
      (Host : GNAT.Sockets.Inet_Addr_Type;
      Port  : GNAT.Sockets.Port_Type);
      
    entry Stop;
  end Server_Task;
  
end Ada_Mr.Generics.Server;