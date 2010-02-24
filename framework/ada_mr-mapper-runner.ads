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
--    Provides the runner package for the Ada_Mr.Mapper component. This package
--    contains the mapper logic.
--  </PURPOSE>
-------------------------------------------------------------------------------

with GNAT.Sockets;
use GNAT.Sockets;
with Ada.Strings.Unbounded;

with Ada_Mr.Generics.Runner;
with Ada_Mr.Helper;
with Ada_Mr.Xml;


generic
  type My_Job is private;
  with function From_Xml
    (Xml_Node : Ada_Mr.Xml.Node_Access)
    return My_Job;
  with function To_Xml
    (Job : in My_Job)
    return String;
  with function Get_Job_Id
    (Job : in My_Job)
    return Natural;
  with procedure Compute_Job
    (Job : in My_Job);
  with function Split_Result_For_Different_Reducer
    return Ada_Mr.Helper.String_String_Maps.Map;
  with procedure Stop_Mapper;
    
package Ada_Mr.Mapper.Runner is  
  
  package ASU renames Ada.Strings.Unbounded;
  
  procedure Run;
  
  package Runner is new Ada_Mr.Generics.Runner(
    Run
  );

end Ada_Mr.Mapper.Runner;
