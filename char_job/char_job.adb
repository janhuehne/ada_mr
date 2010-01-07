with Ada.Text_IO;
with Utility;
with Xml_Helper;
with Logger;

package body Char_Job is
  
  function To_Xml(Job : in My_Job) return String is
    Details : Utility.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Utility.Trim(Job.Job_Id'Img));
    Details.Insert("computable_string", ASU.To_String(Job.Computable_String));
    Details.Insert("responsible_reducer", Job.Responsible_Reducer);
    
    return Xml_Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  function From_Xml(Xml_Node : Xml.Node_Access) return My_Job is
    J : My_Job;
  begin
    J.Job_Id              := Integer'Value(Xml.Get_Value(Xml_Node, "job_id"));
    J.Computable_String   := ASU.To_Unbounded_String(Xml.Get_Value(Xml_Node, "computable_string"));
    J.Responsible_Reducer := Xml.Get_Value(Xml_Node, "responsible_reducer");
    
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
    Utility.Put(Job.Job_Id'Img, 10, 1);
    Utility.Put(ASU.To_String(Job.Computable_String), 30, 1);
    Utility.Put(Job.Responsible_Reducer, 20, 1);
    Utility.Put(State, 20);
    Ada.Text_IO.New_Line;
  end Print_Job;
  
  procedure Compute_Job(Job : in My_Job) is
    Computable_String : String := ASU.To_String(Job.Computable_String);
    Element_Cursor : Utility.String_Integer_Maps.Cursor;
  begin
    
    for I in Computable_String'First .. Computable_String'Last loop
      Element_Cursor := Result_Hash.Find(Computable_String(I..I));
      if Utility.String_Integer_Maps."/="(Element_Cursor, Utility.String_Integer_Maps.No_Element) then
        Result_Hash.Replace_Element(
          Element_Cursor, 
          Utility.String_Integer_Maps.Element(Element_Cursor) + 1
        );
      else
        Result_Hash.Insert(Computable_String(I..I), 1);
      end if;
    end loop;
    
  exception
    when Error : others => raise Utility.Compute_Job_Error;
  end Compute_Job;
  
  function Job_Result_To_Xml return String is
    Result_Cursor : Utility.String_Integer_Maps.Cursor := Utility.String_Integer_Maps.First(Result_Hash);
    Result_String : Ada.Strings.Unbounded.Unbounded_String;
  begin
    while Utility.String_Integer_Maps.Has_Element(Result_Cursor) loop
      Ada.Strings.Unbounded.Append(Result_String, "<");
      Ada.Strings.Unbounded.Append(Result_String, Utility.String_Integer_Maps.Key(Result_Cursor));
      Ada.Strings.Unbounded.Append(Result_String, ">");
      Ada.Strings.Unbounded.Append(Result_String, Utility.Trim(Utility.String_Integer_Maps.Element(Result_Cursor)'Img));
      Ada.Strings.Unbounded.Append(Result_String, "</");
      Ada.Strings.Unbounded.Append(Result_String, Utility.String_Integer_Maps.Key(Result_Cursor));
      Ada.Strings.Unbounded.Append(Result_String, ">");
      
      Utility.String_Integer_Maps.Next(Result_Cursor);
    end loop;
    
    Utility.String_Integer_Maps.Clear(Result_Hash);
    
    return ASU.To_String(Result_String);
  end Job_Result_To_Xml;
  
  procedure Merge_Jobs(Xml_Node : Xml.Node_Access) is
    Cursor : Xml.Node_Access_Vector.Cursor := Xml_Node.Children.First;
  begin
    loop
      exit when Xml.Node_Access_Vector."="(Cursor, Xml.Node_Access_Vector.No_Element);
      
      declare
        Map_Cursor : Utility.String_Integer_Maps.Cursor := Utility.String_Integer_Maps.Find(
          Result_Hash, 
          ASU.To_String(Xml.Node_Access_Vector.Element(Cursor).Tag)
        );
      begin
        if Utility.String_Integer_Maps."="(Map_Cursor, Utility.String_Integer_Maps.No_Element) then
          Result_Hash.Insert(
            ASU.To_String(Xml.Node_Access_Vector.Element(Cursor).Tag),
            Integer'Value(ASU.To_String(Xml.Node_Access_Vector.Element(Cursor).Value))
          );
        else
          
          Result_Hash.Replace_Element(
            Map_Cursor,
            Utility.String_Integer_Maps.Element(Map_Cursor) + Integer'Value(ASU.To_String(Xml.Node_Access_Vector.Element(Cursor).Value))
          );
        end if;
      end;

      Xml.Node_Access_Vector.Next(Cursor);
    end loop;
    
  end Merge_Jobs;
  
  procedure Finalize is
    Cursor : Utility.String_Integer_Maps.Cursor := Result_Hash.First;
  begin
    
    Ada.Text_IO.Put_Line("Reducer result:");
    
    loop
      exit when Utility.String_Integer_Maps."="(Cursor, Utility.String_Integer_Maps.No_Element);
      
      Ada.Text_IO.Put("  ");
      Ada.Text_IO.Put(Utility.String_Integer_Maps.Key(Cursor));
      Ada.Text_IO.Put(": ");
      Ada.Text_IO.Put(Utility.String_Integer_Maps.Element(Cursor)'Img);
      Ada.Text_IO.New_Line;
      
      Utility.String_Integer_Maps.Next(Cursor);
    end loop;
    
  end Finalize;
  
  procedure Split_Raw_Data is
    First : Natural := Complete_String'First;
    Last  : Natural;
    Step  : Natural := 10;
  begin
    Logger.Put_Line("Splitting raw data into jobs", Logger.Info);
    
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
    
    Logger.Put_Line("--> Done", Logger.Info);
  end Split_Raw_Data;
  
  
  function Get_Next_Raw_Job return My_Job is
    J : My_Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
end Char_Job;