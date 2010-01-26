with Ada.Text_IO;
with Ada_Mr.Helper;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Logger;
with Ada.Strings.Maps;
with Ada_Mr.Xml.Helper;
with Ada.Characters.Handling;
with Ada_Mr.Logger;

package body Char_Job is
  
  overriding function To_Xml(The_Job : Char_Job) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Ada_Mr.Helper.Trim(The_Job.Job_Id'Img));
    Details.Insert("computable_string", "#" & ASU.To_String(The_Job.Computable_String) & "#");
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Char_Job is
    J : Char_Job;
  begin
    J.Job_Id              := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "job_id"));
    J.Computable_String   := ASU.Trim(ASU.To_Unbounded_String(Ada_Mr.Xml.Get_Value(Xml_Node, "computable_string")), Ada.Strings.Maps.To_Set("#"), Ada.Strings.Maps.To_Set("#"));
    
    return J;
  end From_Xml;
  
  
  overriding procedure Print_Job(The_Job : Char_Job; State : String) is
  begin
    Ada_Mr.Helper.Put(The_Job.Job_Id'Img, 10, 1);
    Ada_Mr.Helper.Put(ASU.To_String(The_Job.Computable_String), 30, 1);
    Ada_Mr.Helper.Put(State, 20);
    Ada.Text_IO.New_Line;
  end Print_Job;
  
  
  overriding procedure Compute_Job(The_Job : Char_Job) is
    Computable_String : String := ASU.To_String(The_Job.Computable_String);
    Element_Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor;
    Ascii_Code : Integer;
  begin
    Ada_Mr.Logger.Put_Line("Analysing: " & Computable_String, Ada_Mr.Logger.Info);
    
    for I in Computable_String'First .. Computable_String'Last loop
--      Ada_Mr.Logger.Put_Line(Character'Pos(Computable_String(I))'Img, Ada_Mr.Logger.Info);
      Ascii_Code := Character'Pos(Ada.Characters.Handling.To_Upper(Computable_String(I)));
      Element_Cursor := Result_Hash.Find(Ada_Mr.Helper.Trim(Ascii_Code'Img));
      
      if Ada_Mr.Helper.String_Integer_Maps."/="(Element_Cursor, Ada_Mr.Helper.String_Integer_Maps.No_Element) then
        Result_Hash.Replace_Element(
          Element_Cursor,
          Ada_Mr.Helper.String_Integer_Maps.Element(Element_Cursor) + 1
        );
      else
        Result_Hash.Insert(Ada_Mr.Helper.Trim(Ascii_Code'Img), 1);
      end if;
    end loop;
  exception
    when Error : others => 
      raise Ada_Mr.Helper.Compute_Job_Error;
  end Compute_Job;
  
  
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access) is
    Cursor : Ada_Mr.Xml.Node_Access_Vector.Cursor := Xml_Node.Children.First;
    Char   : Character;
    Count  : Natural;
  begin
    loop
      exit when Ada_Mr.Xml.Node_Access_Vector."="(Cursor, Ada_Mr.Xml.Node_Access_Vector.No_Element);
      
      declare
        Map_Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor;
      begin
        Char  := Character'Val(Integer'Value(ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Tag)));
        Count := Natural'Value(ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Value));
        
        Map_Cursor := Ada_Mr.Helper.String_Integer_Maps.Find(
          Result_Hash,
          Character'Image(Char)
        );
        
        if Ada_Mr.Helper.String_Integer_Maps."="(Map_Cursor, Ada_Mr.Helper.String_Integer_Maps.No_Element) then
          Result_Hash.Insert(
            Character'Image(Char),
            Count
          );
        else
          Result_Hash.Replace_Element(
            Map_Cursor,
            Ada_Mr.Helper.String_Integer_Maps.Element(Map_Cursor) + Count
          );
        end if;
      end;
      
      Ada_Mr.Xml.Node_Access_Vector.Next(Cursor);
    end loop;
  end Merge_Job_Results;
  
  
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
    Complete_String : String := Ada_Mr.Helper.Read_Configuration("user", "text");
    First : Natural := Complete_String'First;
    Last  : Natural;
    Step  : Natural := Natural'Value(Ada_Mr.Helper.Read_Configuration("user", "step"));
  begin
    loop
      Last := First + Step - 1;
      
      if Last > Complete_String'Last then
        Last := Complete_String'Last;
      end if;
      
      declare
        Job : Char_Job;
      begin
        Job.Job_Id := Ada_Mr.Job.Get_Next_Job_Id;
        Job.Computable_String := ASU.To_Unbounded_String(Complete_String(First .. Last));
        
        Calculated_Jobs.Append(Job);
      end;
      
      First := Last + 1;
      
      exit when Last = Complete_String'Last;
    end loop;
  end Split_Raw_Data;
  
  
  overriding function Get_Next_Raw_Job return Char_Job is
    J : Char_Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
    Mapping         : Ada_Mr.Helper.String_String_Maps.Map;
    Reducer_Mapping : Ada_Mr.Helper.String_String_Maps.Map;
    
    
    function Job_Result_To_Xml return String is
      Result_Cursor : Ada_Mr.Helper.String_Integer_Maps.Cursor := Ada_Mr.Helper.String_Integer_Maps.First(Result_Hash);
      Result_String : Ada.Strings.Unbounded.Unbounded_String;
    begin
      while Ada_Mr.Helper.String_Integer_Maps.Has_Element(Result_Cursor) loop
        Ada.Strings.Unbounded.Append(Result_String, "<" & Ada_Mr.Helper.String_Integer_Maps.Key(Result_Cursor) & ">");
        Ada.Strings.Unbounded.Append(Result_String, Ada_Mr.Helper.Trim(Ada_Mr.Helper.String_Integer_Maps.Element(Result_Cursor)'Img));
        Ada.Strings.Unbounded.Append(Result_String, "</" & Ada_Mr.Helper.String_Integer_Maps.Key(Result_Cursor) & ">");
        
        Ada_Mr.Helper.String_Integer_Maps.Next(Result_Cursor);
      end loop;
      
      Ada_Mr.Helper.String_Integer_Maps.Clear(Result_Hash);
      
      return ASU.To_String(Result_String);
    end Job_Result_To_Xml;
    
  begin
    Ada_Mr.Helper.String_String_Maps.Insert(Mapping, "Reducer_01", Job_Result_To_Xml);
    
    return Mapping;
  end Split_Result_For_Different_Reducer;
--  
  
end Char_Job;