with Ada.Text_IO;
with Ada_Mr.Xml.Helper;
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
package body Pi_Job is
  
  overriding function To_Xml(The_Job : Pi_Job) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Ada_Mr.Helper.Trim(The_Job.Job_Id'Img));
    Details.Insert("random_inital_value", Ada_Mr.Helper.Trim(The_Job.Random_Inital_Value'Img));
    Details.Insert("pairs_per_map_process", Ada_Mr.Helper.Trim(The_Job.Pairs_Per_Map_Process'Img));
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Pi_Job is
    J : Pi_Job;
  begin
     J.Job_Id                := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "job_id"));
     J.Random_Inital_Value   := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "random_inital_value"));
     J.Pairs_Per_Map_Process := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "pairs_per_map_process"));
     
     return J;
  end From_Xml;
  
  
  procedure Split_Raw_Data is
    type Rand_Range is range 1 .. 999999999;
    package Rand_Int is new Ada.Numerics.Discrete_Random(Rand_Range);
    
    G : Rand_Int.Generator;
    Number_Of_Jobs : Natural;
    Pairs_Per_Map_Process : Natural := 1;
  begin
    Number_Of_Jobs := 40;
    
    declare
    begin
      Pairs_Per_Map_Process := Natural'Value(
        Ada_Mr.Helper.Read_Configuration("user", "pairs_per_map_process")
      );
    exception
      when others => null;
    end;
    
    Rand_Int.Reset(G);
    
    for N in 1 .. Number_Of_Jobs loop
      declare
        Job : Pi_Job;
      begin
        Job.Job_Id := Ada_Mr.Job.Get_Next_Job_Id;
        Job.Random_Inital_Value := Natural(Rand_Int.Random(G));
        Job.Pairs_Per_Map_Process := Pairs_Per_Map_Process;
        
        Calculated_Jobs.Append(Job);
      end;
    end loop;
  end Split_Raw_Data;
  
  
  overriding function Get_Next_Raw_Job return Pi_Job is
    J : Pi_Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
  overriding procedure Print_Job(The_Job : Pi_Job; State : String) is
  begin
    Ada_Mr.Helper.Put(The_Job.Job_Id'Img, 10, 1);
    Ada_Mr.Helper.Put(The_Job.Random_Inital_Value'Img, 30, 1);
    Ada_Mr.Helper.Put(The_Job.Pairs_Per_Map_Process'Img, 30, 1);
    Ada_Mr.Helper.Put(State, 20);
    Ada.Text_IO.New_Line;
  end Print_Job;
  
  
  overriding procedure Compute_Job(The_Job : Pi_Job) is
    G : Ada.Numerics.Float_Random.Generator;
    F1, F2, Phytagoras : Float;
  begin
    In_Circle_Count := 0;
    Not_In_Circle_Count := 0;
    
    Ada.Numerics.Float_Random.Reset(G, The_Job.Random_Inital_Value);
    
    for N in 1 .. The_Job.Pairs_Per_Map_Process loop
      F1 := Ada.Numerics.Float_Random.Random(G);
      F2 := Ada.Numerics.Float_Random.Random(G);
      
      Phytagoras := Float_Functions.Sqrt(F1*F1 + F2*F2);
      if Phytagoras < 1.0 then
        In_Circle_Count := In_Circle_Count + 1;
      else
        Not_In_Circle_Count := Not_In_Circle_Count + 1;
      end if;
    end loop;
  end Compute_Job;
  
  
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
    Mapping : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Mapping.Insert(
      "Reducer_01", 
      "<in_circle_count>" & Ada_Mr.Helper.Trim(In_Circle_Count'Img) & "</in_circle_count><not_in_circle_count>" & Ada_Mr.Helper.Trim(Not_In_Circle_Count'Img) & "</not_in_circle_count>"
    );
    
    return Mapping;
  end Split_Result_For_Different_Reducer;
  
  
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access) is
  begin
    In_Circle_Count := In_Circle_Count + Natural'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "in_circle_count"));
    Not_In_Circle_Count := Not_In_Circle_Count + Natural'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "not_in_circle_count"));
  end Merge_Job_Results;
  
  
  procedure Finalize is
    Pi : Float;
  begin
    Pi := 4.0 * Float(In_Circle_Count) / Float(In_Circle_Count + Not_In_Circle_Count);
    
    Ada.Text_IO.Put_Line("Reducer result: ");
    Ada.Text_IO.Put_Line("In circle    : " & In_Circle_Count'Img);
    Ada.Text_IO.Put_Line("Not in circle: " & Not_In_Circle_Count'Img);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put_Line("Calculated Pi: " & Float'Image(Pi));
  end Finalize;
  
end Pi_Job;