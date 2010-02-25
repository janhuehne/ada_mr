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
--    Provides the main package for the Ada_Mr.Reducer component.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada_Mr;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada_Mr.Xml;
with Ada_Mr.Generics.Console;
with Ada_Mr.Reducer.Server;
with Ada_Mr.Reducer.Runner;
with Ada_Mr.Generics.Runner;

generic
  with procedure Merge_Jobs
    (Xml_Node    : Ada_Mr.Xml.Node_Access; 
     Stop_System : out Boolean);
  with procedure Finalize_Jobs;
  
package Ada_Mr.Reducer.Main is
  
  package ASU renames Ada.Strings.Unbounded;
  
  
  
  
  type Reducer_Task;
  type Reducer_Task_Access is access Reducer_Task;
  
  task type Reducer_Task is
    entry Start(Self : Reducer_Task_Access);
    entry Stop;
  end Reducer_Task;

  procedure Stop_Reducer_Task;
  -- Stops the reducer.
  
  
  procedure Merge_Mapper_Results;
  -- Merges a mapper result.
    
  package Result_Merge is new Ada_Mr.Generics.Runner(
    Merge_Mapper_Results
  );
  
  
  
  
  package Server is new Ada_Mr.Reducer.Server(
    Finalize_Jobs,
    Stop_Reducer_Task
  );
  
  
  
  
  package Runner is new Ada_Mr.Reducer.Runner(
    Stop_Reducer_Task
  );
  
private  
  Main_Task : Reducer_Task_Access;
end Ada_Mr.Reducer.Main;