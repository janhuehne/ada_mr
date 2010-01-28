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
    Equal : String := "00000000";
    Counter : Natural := 0;
  begin
    Hash := The_Job.Start_Point;
    
    loop
      Tmp  := Hash;
      Hash := GNAT.MD5.Digest(Tmp);
      
      for I in Equals'Range loop
        
        Step_Counter(I) := Step_Counter(I) + 1;
        
        if Hash((32 - Equals(I)'Length + 1) .. 32) = Equals(I) then
          Current_Distinguished_Point  := Hash;
          
          Ada.Text_IO.Put_Line("Last distinguished point:    " & Last_Distinguished_Points(I));
          Ada.Text_IO.Put_Line("Current distinguished point: " & Current_Distinguished_Point);
          Ada.Text_IO.Put_Line("Steps between:               " & Step_Counter(I)'Img);
          
          -- --> Send result to reducers
          declare
            Reducer_Result_Map : Ada_Mr.Helper.String_String_Maps.Map;
          begin 
            Reducer_Result_Map := Split_Result_For_Different_Reducer;
            Reducer_Result_Map.Iterate(Ada_Mr.Mapper.Helper.Send_Result_To_Reducer'Access);
          end;
          
          
          
          
          Last_Distinguished_Points(I) := Current_Distinguished_Point;
          Step_Counter(I) := 0;
        end if;
        
      end loop;
        
--        Ada.Text_IO.Put_Line("Last distinguished point:    " & Last_Distinguished_Point);
--        Ada.Text_IO.Put_Line("Current distinguished point: " & Current_Distinguished_Point);
--        Ada.Text_IO.Put_Line("Steps between:               " & Counter'Img);
    end loop;
      
  end Compute_Job;
  
  
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
    Mapping  : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    -- Example:
    --  Mapping.Insert(
    --    "Reducer_1", 
    --    ""
    --  );
    
    return Mapping;
  end Split_Result_For_Different_Reducer;
  
  
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean) is
  begin
    null;
  end Merge_Job_Results;
  
  
  procedure Finalize is
  begin
    null;
  end Finalize;
  
end Md5_Job;
