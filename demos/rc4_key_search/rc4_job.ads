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

with Rc_4;

package Rc4_Job is
  
  -- Package rename
  package ASU renames Ada.Strings.Unbounded;
  
  
  -- Job record definition
  type Job is new Ada_Mr.Job.Object with record
    Plain_Text          : Rc_4.Unsigned_Byte_Array(1 .. 10);
    Cipher_Text         : Rc_4.Unsigned_Byte_Array(1 .. 10);
    Start_Most_Sig_Byte : Rc_4.Unsigned_Byte;
    Most_Sig_Byte_Range : Natural;
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
  
  
  Key_Found : Boolean := False;
  Found_Key : Rc_4.Key_Type;
private

  function Byte_Array_To_Xml(Byte_Array : Rc_4.Unsigned_Byte_Array) return String ;
  procedure Byte_Array_From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access; Byte_Array : out Rc_4.Unsigned_Byte_Array);
end Rc4_Job;
