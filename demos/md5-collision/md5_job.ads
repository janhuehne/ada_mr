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
    Start_Point : GNAT.MD5.Message_Digest;
  end record;
  
  
  -- xml stuff
  overriding function To_Xml(The_Job : Job) return String;
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Job;
  
  
  -- split and get raw data
  procedure Split_Raw_Data;
  overriding function Get_Next_Raw_Job return Job;
  
  
  -- print job on stdio (optional)
  overriding procedure Print_Job(The_Job : Job; State : String; Message : String);
  
  
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
  
  
  
  -- Datastructure and content for the distinguished point pattern
  Dp_Pattern_Length : Natural := 6;
  
  type Dp_Pattern_Array  is array(Natural range <>) of String(1 .. Dp_Pattern_Length);
  Dp_Pattern : Dp_Pattern_Array := ("000000", "111111", "222222", "333333", "444444", "555555", "666666", "777777", "888888", "999999");
  
  
  
  Collision_Length : Natural := 14;
  
  
  
  -- Datastructe for the distinguished point set
  type Distinguished_Point_Set is record
    Last     : GNAT.MD5.Message_Digest;
    Current  : GNAT.MD5.Message_Digest;
    Distance : Natural;
  end record;
  
  function Distinguished_Point_Set_To_Xml(Set : Distinguished_Point_Set) return String;
  function Distinguished_Point_Set_From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Distinguished_Point_Set;
  procedure Print(Set : Distinguished_Point_Set);
  
  function Calculate_Collision(In_Dp_1 : Distinguished_Point_Set; In_Dp_2 : Distinguished_Point_Set) return Boolean;
  
  -- Mapper Stuff
  type Distinguished_Point_Set_Array is array(Natural range <>) of Distinguished_Point_Set;
  Distinguished_Point : Distinguished_Point_Set_Array(Dp_Pattern'Range);
  Result_To_Send : Natural;
  
  
  -- Reducer Stuff
  package D_P_Vector is new Ada.Containers.Vectors(
    Element_Type => Distinguished_Point_Set, 
    Index_Type => Positive
  );
  
  Distinguished_Points : D_P_Vector.Vector;
  
  
  Null_String : String := "00000000000000000000000000000000";
end Md5_Job;
