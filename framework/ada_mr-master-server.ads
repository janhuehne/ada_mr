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
--    Provides the server package for the Ada_Mr.Master component. This package
--    contains the local server to handle incomming requests.
--  </PURPOSE>
-------------------------------------------------------------------------------

with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Master.Helper;
with Ada.Strings.Unbounded;
with Ada_Mr.Generics.Server;
with Ada_Mr.Generics.Echo;
with Ada_Mr.Xml;
with Ada_Mr.Helper;

generic
  type My_Job is private;
  type Job_Entry_Record_Access is private;
  with function From_Xml
    (Xml_Node : Ada_Mr.Xml.Node_Access)
    return My_Job;
  with procedure Add_Worker
    (New_Worker : in out Ada_Mr.Master.Helper.Worker_Record_Access);
  with function Find_Worker_By_Identifier
    (Identifier : String)
    return Ada_Mr.Master.Helper.Worker_Record_Access;
  with function Find_Worker_By_Access_Token_And_Type
    (Access_Token : String;
     W_Type       : Ada_Mr.Helper.Worker_Type)
    return Ada_Mr.Master.Helper.Worker_Record_Access;
  with procedure Add_Job
    (Job : My_Job);
  with function Get_Job_By_Id
    (Id : Natural)
    return Job_Entry_Record_Access;
  with function Get_Next_Pending_Job
    return Job_Entry_Record_Access;
  with procedure Change_Job_State
    (Job_Entry : in out Job_Entry_Record_Access;
     State     : Ada_Mr.Master.Helper.Job_State;
     Message   : String := "");
  with function Job_Entry_To_Xml
    (Job_Entry : Job_Entry_Record_Access)
    return String;
  with procedure Stop_Master;
    
package Ada_Mr.Master.Server is
  
  package ASU renames Ada.Strings.Unbounded;
  
  function Exit_Server
    return Boolean;
  -- Returns <code>True</code> if the server should terminate.
  
  procedure Process_Request
    (S        : Stream_Access; 
     From     : Ada_Mr.Helper.Worker_Type;
     Xml_Root : Ada_Mr.Xml.Node_Access);
  -- Processes an in comming request.
  
  package Server is new Ada_Mr.Generics.Server(
    Exit_Server,
    Process_Request
  );
  
end Ada_Mr.Master.Server;