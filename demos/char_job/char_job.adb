with Ada.Text_IO;
with Ada_Mr.Helper;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Logger;
with Ada.Strings.Maps;

package body Char_Job is
  
  function To_Xml(Job : in My_Job) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Ada_Mr.Helper.Trim(Job.Job_Id'Img));
    Details.Insert("computable_string", "#" & ASU.To_String(Job.Computable_String) & "#");
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return My_Job is
    J : My_Job;
  begin
    J.Job_Id              := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "job_id"));
    J.Computable_String   := ASU.Trim(ASU.To_Unbounded_String(Ada_Mr.Xml.Get_Value(Xml_Node, "computable_string")), Ada.Strings.Maps.To_Set("#"), Ada.Strings.Maps.To_Set("#"));
    
    return J;
  end From_Xml;
  
  function Get_Job_Id(Job : My_Job) return Natural is
  begin
    return Job.Job_Id;
  end Get_Job_Id;
  
  procedure Split_Data_Into_Jobs(Process : Add_Job_Procedure) is
    First : Natural := Complete_String'First;
    Last  : Natural;
    Step  : Natural := 10;
  begin
    
    loop
      Last := First + Step - 1;
      
      if Last > Complete_String'Last then
        Last := Complete_String'Last;
      end if;
      
      declare
        Job : My_Job;
      begin
        Job.Job_Id := Get_Next_Job_Counter;
        Job.Computable_String := ASU.To_Unbounded_String(Complete_String(First .. Last));
        Job.Length := Last - First + 1;
        
        Process(Job);
      end;
      
      First := Last + 1;
      
      exit when Last = Complete_String'Last;
    end loop;
    
  end Split_Data_Into_Jobs;
  
  function Get_Next_Job_Counter(Auto_Inc : Boolean := true) return Natural is
    Return_Value : Natural := Job_Counter;
  begin
    if Auto_Inc = true then
      Job_Counter := Job_Counter + 1;
    end if;
      
    return Return_Value;
  end Get_Next_Job_Counter;
  
  procedure Print_Job(Job : in My_Job; State : String) is
  begin
    Ada_Mr.Helper.Put(Job.Job_Id'Img, 10, 1);
    Ada_Mr.Helper.Put(ASU.To_String(Job.Computable_String), 30, 1);
    Ada_Mr.Helper.Put(State, 20);
    Ada.Text_IO.New_Line;
  end Print_Job;
  
  procedure Compute_Job(Job : in My_Job) is
    Computable_String : String := ASU.To_String(Job.Computable_String);
    Element_Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor;
    
    function Special_Char(Input : String) return String is
    begin
      if Input = " " or Input = "." or Input = "," or Input = ";" then
        return "SZ";
      else
        return Input;
      end if;
    end;
  begin
    Ada_Mr.Logger.Put_Line("Analysing: " & Computable_String, Ada_Mr.Logger.Info);
    
    for I in Computable_String'First .. Computable_String'Last loop
      Element_Cursor := Result_Hash.Find(Special_Char(Computable_String(I..I)));
      if Ada_Mr.Helper.String_Integer_Maps."/="(Element_Cursor, Ada_Mr.Helper.String_Integer_Maps.No_Element) then
        Result_Hash.Replace_Element(
          Element_Cursor, 
          Ada_Mr.Helper.String_Integer_Maps.Element(Element_Cursor) + 1
        );
      else
        Result_Hash.Insert(Special_Char(Computable_String(I..I)), 1);
      end if;
    end loop;
    
  exception
    when Error : others => raise Ada_Mr.Helper.Compute_Job_Error;
  end Compute_Job;
  
  function Job_Result_To_Xml return String is
    Result_Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor := Ada_Mr.Helper.String_Integer_Maps.First(Result_Hash);
    Result_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    while Ada_Mr.Helper.String_Integer_Maps.Has_Element(Result_Cursor) loop
      Ada.Strings.Unbounded.Append(Result_String, "<");
      Ada.Strings.Unbounded.Append(Result_String, Ada_Mr.Helper.String_Integer_Maps.Key(Result_Cursor));
      Ada.Strings.Unbounded.Append(Result_String, ">");
      Ada.Strings.Unbounded.Append(Result_String, Ada_Mr.Helper.Trim(Ada_Mr.Helper.String_Integer_Maps.Element(Result_Cursor)'Img));
      Ada.Strings.Unbounded.Append(Result_String, "</");
      Ada.Strings.Unbounded.Append(Result_String, Ada_Mr.Helper.String_Integer_Maps.Key(Result_Cursor));
      Ada.Strings.Unbounded.Append(Result_String, ">");
      
      Ada_Mr.Helper.String_Integer_Maps.Next(Result_Cursor);
    end loop;
    
    Ada_Mr.Helper.String_Integer_Maps.Clear(Result_Hash);
    
    return ASU.To_String(Result_String);
  end Job_Result_To_Xml;
  
  procedure Merge_Jobs(Xml_Node : Ada_Mr.Xml.Node_Access) is
    Cursor : Ada_Mr.Xml.Node_Access_Vector.Cursor := Xml_Node.Children.First;
  begin
    loop
      exit when Ada_Mr.Xml.Node_Access_Vector."="(Cursor, Ada_Mr.Xml.Node_Access_Vector.No_Element);
      
      declare
        Map_Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor := Ada_Mr.Helper.String_Integer_Maps.Find(
          Result_Hash, 
          ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Tag)
        );
      begin
        if Ada_Mr.Helper.String_Integer_Maps."="(Map_Cursor, Ada_Mr.Helper.String_Integer_Maps.No_Element) then
          Result_Hash.Insert(
            ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Tag),
            Integer'Value(ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Value))
          );
        else
          
          Result_Hash.Replace_Element(
            Map_Cursor,
            Ada_Mr.Helper.String_Integer_Maps.Element(Map_Cursor) + Integer'Value(ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Value))
          );
        end if;
      end;

      Ada_Mr.Xml.Node_Access_Vector.Next(Cursor);
    end loop;
    
  end Merge_Jobs;
  
  procedure Finalize is
    Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor := Result_Hash.First;
  begin
    
    Ada.Text_IO.Put_Line("Reducer result:");
    
    loop
      exit when Ada_Mr.Helper.String_Integer_Maps."="(Cursor, Ada_Mr.Helper.String_Integer_Maps.No_Element);
      
      Ada.Text_IO.Put("  ");
      Ada.Text_IO.Put(Ada_Mr.Helper.String_Integer_Maps.Key(Cursor));
      Ada.Text_IO.Put(": ");
      Ada.Text_IO.Put(Ada_Mr.Helper.String_Integer_Maps.Element(Cursor)'Img);
      Ada.Text_IO.New_Line;
      
      Ada_Mr.Helper.String_Integer_Maps.Next(Cursor);
    end loop;
    
  end Finalize;
  
  procedure Split_Raw_Data is
    First : Natural := Complete_String'First;
    Last  : Natural;
    Step  : Natural := 10;
  begin
    Ada_Mr.Logger.Put_Line("Splitting raw data into jobs", Ada_Mr.Logger.Info);
    
    loop
      Last := First + Step - 1;

      if Last > Complete_String'Last then
        Last := Complete_String'Last;
      end if;

      declare
        Job : My_Job;
      begin
        Job.Job_Id := Get_Next_Job_Counter;
        Job.Computable_String := ASU.To_Unbounded_String(Complete_String(First .. Last));
        Job.Length := Last - First + 1;
        
        Calculated_Jobs.Append(Job);
      end;

      First := Last + 1;

      exit when Last = Complete_String'Last;
    end loop;
    
    Ada_Mr.Logger.Put_Line("--> Done", Ada_Mr.Logger.Info);
  end Split_Raw_Data;
  
  
  function Get_Next_Raw_Job return My_Job is
    J : My_Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
    Mapping         : Ada_Mr.Helper.String_String_Maps.Map;
    Reducer_Mapping : Ada_Mr.Helper.String_String_Maps.Map;
    
  begin
    Ada_Mr.Helper.String_String_Maps.Insert(Mapping, "Reducer_01", Job_Result_To_Xml);
    
    return Mapping;
  end Split_Result_For_Different_Reducer;
  
  
end Char_Job;