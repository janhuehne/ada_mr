with Ada.Text_IO;
with Ada_Mr.Logger;
with Ada_Mr.Xml.Helper;
with Ada.Numerics.Discrete_Random;
with Ada_Mr.Mapper.Helper;

package body Md5_Job is
  
  overriding function To_Xml(The_Job : Job) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Ada_Mr.Helper.Trim(The_Job.Job_Id'Img));
    Details.Insert("start_point", The_Job.Start_Point);
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Job is
    J : Job;
  begin
    J.Job_Id := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "job_id"));
    J.Start_Point := Ada_Mr.Xml.Get_Value(Xml_Node, "start_point");
    
    return J;
  end From_Xml;
  
  
  procedure Split_Raw_Data is
    subtype Rand_Range is Integer range 65..90;
    package Rand is new Ada.Numerics.Discrete_Random(Rand_Range);
    Gen : Rand.Generator;
  begin
    Rand.Reset(Gen);
    
    for I in 1 .. 10 loop
      
      declare
        The_Job : Job;
      begin
        The_Job.Job_Id := Ada_Mr.Job.Get_Next_Job_Id;
        
        for I in The_Job.Start_Point'Range loop
          The_Job.Start_Point(I) := Character'Val(Rand.Random(Gen));
        end loop;
          
        The_Job.Start_Point := GNAT.MD5.Digest(The_Job.Start_Point);
        
        Calculated_Jobs.Append(The_Job);
      end;
    end loop;
  end Split_Raw_Data;
  
  
  overriding function Get_Next_Raw_Job return Job is
    J : Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
  overriding procedure Print_Job(The_Job : Job; State : String) is
  begin
    Ada_Mr.Helper.Put(The_Job.Job_Id'Img, 10, 1);
    Ada_Mr.Helper.Put(The_Job.Start_Point, 50, 1);
    Ada_Mr.Helper.Put(State, 20);
    Ada.Text_IO.New_Line;
  end Print_Job;
  
  
overriding procedure Compute_Job(The_Job : Job) is
  Hash  : GNAT.MD5.Message_Digest;
  Tmp   : GNAT.MD5.Message_Digest;
begin
  Hash := The_Job.Start_Point;
  
  for I in Distinguished_Point'Range loop
    Distinguished_Point(I).Last := Hash;
  end loop;
  
  loop
    Tmp  := Hash;
    Tmp(1 .. (32-Collision_Length)) := Null_String(1 .. (32-Collision_Length));
    Hash := GNAT.MD5.Digest(Tmp);
    
    for I in Dp_Pattern'Range loop
      
      Distinguished_Point(I).Distance := Distinguished_Point(I).Distance + 1;
      
      if Hash((32 - Dp_Pattern_Length + 1) .. 32) = Dp_Pattern(I) then
        Result_To_Send := I;
        
        Distinguished_Point(I).Current := Hash;
        
        -- --> Send result to reducers
        Ada_Mr.Mapper.Helper.Send_Result(Split_Result_For_Different_Reducer);
        
        Distinguished_Point(I).Last     := Distinguished_Point(I).Current;
        Distinguished_Point(I).Distance := 0;
      end if;
      
    end loop;
  end loop;
end Compute_Job;
  
  
function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
  Mapping  : Ada_Mr.Helper.String_String_Maps.Map;
begin
  if Result_To_Send <= 5 then
    Mapping.Insert(
      "Reducer_1",
      Distinguished_Point_Set_To_Xml(Distinguished_Point(Result_To_Send))
    );
  else
    Mapping.Insert(
      "Reducer_2",
      Distinguished_Point_Set_To_Xml(Distinguished_Point(Result_To_Send))
    );
  end if;
  
  return Mapping;
end Split_Result_For_Different_Reducer;
  
  
procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean) is
  Current_Dp_Set : Distinguished_Point_Set;
  Distinguished_Points_Cursor : D_P_Vector.Cursor := Distinguished_Points.First;
  Stop : Boolean := False;
begin
  Current_Dp_Set := Distinguished_Point_Set_From_Xml(Xml_Node);
  
  Ada.Text_IO.Put_Line("Calling Merge Job Result");
  
  loop
    Ada.Text_IO.Put_Line("Looping");
    exit when D_P_Vector."="(Distinguished_Points_Cursor, D_P_Vector.No_Element);
      
    if D_P_Vector.Element(Distinguished_Points_Cursor).Current((32-Collision_Length+1) .. 32) = Current_Dp_Set.Current((32-Collision_Length+1) .. 32) then
      Ada.Text_IO.Put_Line("Collision candidate found!");
      if Calculate_Collision(D_P_Vector.Element(Distinguished_Points_Cursor), Current_Dp_Set) = True then
        Stop := True;
        exit;
      end if;
    end if;
    
    D_P_Vector.Next(Distinguished_Points_Cursor);
  end loop;
  
  Ada.Text_IO.Put_Line("End loop");
  
  if D_P_Vector."="(Distinguished_Points_Cursor, D_P_Vector.No_Element) then
    Distinguished_Points.Append(Current_Dp_Set);
  end if;
  
  if Stop = True then
    Stop_System := True;
  end if;
exception
  when Error : others => Ada_Mr.Helper.Print_Exception(Error);
end Merge_Job_Results;
  
  
procedure Finalize is
begin
  Ada.Text_IO.Put_Line("Some reducer found a collision. Shuting down!");
end Finalize;
  
  
  function Distinguished_Point_Set_To_Xml(Set : Distinguished_Point_Set) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("ldp", Set.Last);
    Details.Insert("cdp", Set.Current);
    Details.Insert("distance", Ada_Mr.Helper.Trim(Set.Distance'Img));
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end;
  
  
  function Distinguished_Point_Set_From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Distinguished_Point_Set is
    S : Distinguished_Point_Set;
  begin
    S.Last     := Ada_Mr.Xml.Get_Value(Xml_Node, "ldp");
    S.Current  := Ada_Mr.Xml.Get_Value(Xml_Node, "cdp");
    S.Distance := Natural'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "distance"));
    
    return S;
  end Distinguished_Point_Set_From_Xml;
  
  procedure Print(Set : Distinguished_Point_Set) is
  begin
    Ada.Text_IO.Put_Line("Last Distinguished Point:    " & Set.Last);
    Ada.Text_IO.Put_Line("Current Distinguished Point: " & Set.Current);
    Ada.Text_IO.Put_Line("Distance:                   " & Set.Distance'Img);
  end Print;
  
  
  function Calculate_Collision(In_Dp_1 : Distinguished_Point_Set; In_Dp_2 : Distinguished_Point_Set) return Boolean is
    Dp_1 : Distinguished_Point_Set;
    Dp_2 : Distinguished_Point_Set;
  begin
    if In_Dp_1.Distance > In_Dp_2.Distance then
      Dp_1 := In_Dp_1;
      DP_2 := In_Dp_2;
    else
      Dp_1 := In_Dp_2;
      DP_2 := In_Dp_1;
    end if;
    
    declare
      Md_1, Md_Tmp_1, Md_2, Md_Tmp_2 : GNAT.MD5.Message_Digest;
      Iterations : Natural;
    begin
      -- find common start point
      Md_1 := Dp_1.Last;
      
      for I in 1 .. (Dp_1.Distance - Dp_2.Distance) loop
        Md_1(1 .. (32-Collision_Length)) := Null_String(1 .. (32-Collision_Length));
        Md_Tmp_1 := GNAT.MD5.Digest(Md_1);
        Md_1 := Md_Tmp_1;
      end loop;
      
      Md_2 := Dp_2.Last;
      
      for I in 1 .. Dp_2.Distance loop
        Iterations := I;
        
        Md_Tmp_1 := Md_1;
        Md_Tmp_1(1 .. (32-Collision_Length)) := Null_String(1 .. (32-Collision_Length));
        Md_1 := GNAT.MD5.Digest(Md_Tmp_1);
        
        Md_Tmp_2 := Md_2;
        Md_Tmp_2(1 .. (32-Collision_Length)) := Null_String(1 .. (32-Collision_Length));
        Md_2 := GNAT.MD5.Digest(Md_Tmp_2);
        
        exit when Md_1((32-Collision_Length+1) .. 32) = Md_2((32-Collision_Length+1) .. 32);
      end loop;
      
      if Md_Tmp_1 = Md_Tmp_2 then
        return False;
      end if;
      
      if Md_1((32-Collision_Length+1) .. 32) /= Md_2((32-Collision_Length+1) .. 32) then
        return False;
      end if;
      
      Ada.Text_IO.Put_Line("Collision found:");
      Ada.Text_IO.Put_Line("Iterations:" & Iterations'Img);
      Ada.Text_IO.Put_Line("MD5(" & Md_Tmp_1 & ") = " & Md_1);
      Ada.Text_IO.Put_Line("MD5(" & Md_Tmp_2 & ") = " & Md_2);
      
      return True;
    end;
  end Calculate_Collision;
  
end Md5_Job;
