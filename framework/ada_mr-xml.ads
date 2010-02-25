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
--    Root package for the Ada_Mr.Xml component.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Ada_Mr.Xml is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Node;
  type Node_Access is access Node;
  
  Node_Not_Found : exception;
  
  package Node_Access_Vector is new Ada.Containers.Vectors(
    Element_Type => Node_Access, 
    Index_Type => Positive);
    
  type Node is record
    Tag      : ASU.Unbounded_String;
    Value    : ASU.Unbounded_String;
    Parent   : Node_Access;
    Children : Node_Access_Vector.Vector;
  end record;
  
  procedure Add_Node
    (Root_Node : in out Node_Access;
     Child : in out Node_Access);
  -- Adds the node <code>Child</code> to the <code>Root_Node</code>.
  
  procedure Print
    (Root_Node : in Node_Access;
     Depth : Integer := 0);
  -- Prints the xml tree.
  
  function To_String
    (Root_Node : in Node_Access;
     Depth : Integer := 0)
    return String;
  -- Converts a xml tree to a string.
  
  function Node_Content_To_String
    (Root_Node : in Node_Access)
     return String;
  -- Returns the node content as a string.
  
  function Find_Child_With_Tag
    (Root_Node : in Node_Access; 
     Tag       : in String)
    return Node_Access;
  -- Looks for a <code>Tag</code> in the xml tree <code>Root_Node</code>.
  
  function Get_Value
    (Root : Node_Access;
     Tag : String) 
    return String;
  -- Returns the value of the <code>Tag</code>. If the <code>Tag</code> could not 
  -- found in the <code>Root</code> it raises the <code>Node_Not_Found</code> exception.
  
  function Get_Value_Or_Empty
    (Root : Node_Access; 
     Tag  : String)
    return String;
  -- Returns the value of the <code>Tag</code>. If the <code>Tag</code> could not 
  -- found in the <code>Root</code> it returns <code>""</code>.
  
  function Get_Tag
    (Root : Node_Access)
    return String;
  -- Returns the top node as a string.
  
  function "="(Left, Right : Ada_Mr.Xml.Node_Access) return Boolean;
  -- Returns <code>True</code> if <code>Left = Right</code>.
  
  Wrong_Xml : Exception;
end Ada_Mr.Xml;