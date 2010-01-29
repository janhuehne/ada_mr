-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;
with Ada_Mr.Xml;

with Ada_Mr.Job;

with GNAT.MD5;

package Md5_Job is
  
  -- Package rename
  package ASU renames Ada.Strings.Unbounded;
  
  
  -- Job record definition
  type Job is new Ada_Mr.Job.Object with record
    Start_Point    : String(1..32);
  end record;
  
  
  -- xml stuff
  overriding function To_Xml(The_Job : Job) return String;
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Job;
  
  
  -- split and get raw data
  procedure Split_Raw_Data;
  overriding function Get_Next_Raw_Job return Job;
  
  
  -- print job on stdio (optional)
  overriding procedure Print_Job(The_Job : Job; State : String);
  
  
  -- compute job (map function)
  overriding procedure Compute_Job(The_Job : Job);
  
  
  -- splitting the job result for serval reducers
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  
  
  -- merge job results
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean);
  
  
  -- handle the final reducer results
  procedure Finalize;
  
  
  
  
  -- package instance
  -- Vector to store all computed jobs
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => Job, 
    Index_Type => Positive
  );
  
  
  -- Precalculated jobs
  Calculated_Jobs : Job_Vector.Vector;
  
  
  type D_P_Array is array(Natural range <>) of GNAT.MD5.Message_Digest;
  type Nat_Array is array(Natural range <>) of Natural;
  type Eq_Array  is array(Natural range <>) of String(1..6);
  
  Equals : Eq_Array(1 ..10) := ("000000", "111111", "222222", "333333", "444444", "555555", "666666", "777777", "888888", "999999");
  Last_Distinguished_Points : D_P_Array(Equals'Range);
  Step_Counter : Nat_Array(Equals'Range);
  Current_Distinguished_Point : GNAT.MD5.Message_Digest;
  Result_To_Send : Natural;
  
end Md5_Job;
