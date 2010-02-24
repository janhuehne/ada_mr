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
--    Abstract job package.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada_Mr.Xml;
with Ada_Mr.Helper;

package Ada_Mr.Job is
  
  type Object is abstract tagged record
    Job_Id : Natural := 0;
  end record;
  
  function To_Xml
    (The_Job : Object) 
    return String is abstract;
  -- Serializes a job into the xml format
  
  function From_Xml
    (Xml_Node : Ada_Mr.Xml.Node_Access)
    return Object is abstract;
  -- Deserializes a job from the xml format
  
  function Get_Job_Id
    (The_Job : Object)
    return Natural;
  --  Returns the id from a given job
  
  procedure Split_Raw_Data 
    is abstract;
  -- Splits the raw data into comoputable subjobs
  
  function Get_Next_Raw_Job 
    return Object is abstract;
  -- Is called from the generic master to import the jobs
  
  procedure Print_Job
    (The_Job : Object; 
     State   : String; 
     Message : String);
  -- Called by the generic system to print job in STD/IO
  
  procedure Compute_Job
    (The_Job : Object) is abstract;
  -- Called by the generic mapper to compute a job
  
  function Split_Result_For_Different_Reducer 
    return Ada_Mr.Helper.String_String_Maps.Map is abstract;
  -- Function to split a result for serval reducers
  
  procedure Merge_Job_Results
    (Xml_Node    : Ada_Mr.Xml.Node_Access; 
     Stop_System : out Boolean) is abstract;
  -- Procedure to merge pending job results
  
  procedure Finalize 
    is abstract;
  -- Procedure called by the reducer to handle the job result
  
  function Get_Next_Job_Id 
    return Natural;
  -- Returns the next job id
  
  procedure Set_Job_Id
    (The_Job : in out Object);
  -- Set for the <code>job_id</code>

private
  Job_Counter : Natural := 1;
end Ada_Mr.Job;