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
--    Helper package for the Ada_Mr.Reducer component. Provides a couple 
--    of helper methods.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada_Mr.Xml;
with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Ada_Mr.Reducer.Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  protected Aborted is
    procedure Stop;
    -- Stops the mapper.
    
    function Check
      return Boolean;
    -- Returns <code>True</code> if the reducer must be stopped.
  private
    Abort_It  : Boolean := false;
  end Aborted;
  
  procedure Import_Not_Delivered_Mapper_Results_From_Master;
  -- Imports mapper result from the master, which could not delivered before.
  
  protected Mapper_Results is
    procedure Add
      (Node : Ada_Mr.Xml.Node_Access);
    -- Adds an incomming mapper result.
    
    procedure Add(Node_Vector : Ada_Mr.Xml.Node_Access_Vector.Vector);
    -- Adds a vector with incomming mapper results.
    
    function Is_Empty return Boolean;
    -- Returns <code>True</code> if the mapper results are processed.
      
    function Get_Next return Ada_Mr.Xml.Node_Access;
    -- Returns the next mapper result.
  end Mapper_Results;
  
  Mapper_Results_Vector : Ada_Mr.Xml.Node_Access_Vector.Vector;
  
end Ada_Mr.Reducer.Helper;