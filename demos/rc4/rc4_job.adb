with Ada.Text_IO;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Logger;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;
package body Rc4_Job is
  
  overriding function To_Xml(The_Job : Rc4_Job) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Ada_Mr.Helper.Trim(The_Job.Job_Id'Img));
    Details.Insert("plain_text", Byte_Array_To_Xml(The_Job.Plain_Text));
    Details.Insert("cipher_text", Byte_Array_To_Xml(The_Job.Cipher_Text));
    Details.Insert("start_most_sig_byte", Ada_Mr.Helper.Trim(The_Job.Start_Most_Sig_Byte'Img));
    Details.Insert("most_sig_byte_range", Ada_Mr.Helper.Trim(The_Job.Most_Sig_Byte_Range'Img));
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Rc4_Job is
    J : Rc4_Job;
  begin
    J.Job_Id              := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "job_id"));
    
    Byte_Array_From_Xml(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Node, "plain_text"), J.Plain_Text);
    Byte_Array_From_Xml(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Node, "cipher_text"), J.Cipher_Text);
    
    J.Start_Most_Sig_Byte := Rc_4.Unsigned_Byte(Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "start_most_sig_byte")));
    J.Most_Sig_Byte_Range := Natural'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "most_sig_byte_range"));
    
    return J;
  exception
    when Error : others => Ada_Mr.Helper.Print_Exception(Error);
  end From_Xml;
  
  
  procedure Split_Raw_Data is
    In_Key      : Integer := Integer'Value(Ada_Mr.Helper.Read_Configuration("user", "key"));
    In_Plain    : String  := "abcdefghij";
    
    Random_Key  : Rc_4.Key_Type;
    S_Box       : Rc_4.S_Box;
    
    Plain_Text  : Rc_4.Unsigned_Byte_Array(1 .. 10) := (others => 0);
    Cipher_Text : Rc_4.Unsigned_Byte_Array(1 .. 10) := (others => 0);
    
    Most_Sig_Byte_Range : Natural := Natural'Value(Ada_Mr.Helper.Read_Configuration("user", "most_sig_byte_range"));
  begin
    Random_Key := Rc_4.Random_Key(In_Key);
    
    Rc_4.Print_Key(Random_Key);
--    Random_Key(0) := 0;
--    Random_Key(1) := 0;
--    Random_Key(2) := 200;
--    Random_Key(3) := 0;
    
    Rc_4.Init_RC4(Random_Key, S_Box);
    
    -- plaintext into byte array
    for I in In_Plain'First .. In_Plain'Last loop
      Plain_Text(I) := Character'Pos(In_Plain(I));
    end loop;
    
    Cipher_Text := Plain_Text;
    
    -- encrypt plaintext
    Rc_4.Encrypt(S_Box, Cipher_Text);
    
    for I in 0 .. (255 / Most_Sig_Byte_Range) loop
      
      declare
        Job : Rc4_Job;
      begin
        Job.Job_Id              := Ada_Mr.Job.Get_Next_Job_Id;
        Job.Plain_Text          := Plain_Text;
        Job.Cipher_Text         := Cipher_Text;
        Job.Start_Most_Sig_Byte := Rc_4.Unsigned_Byte((I * Most_Sig_Byte_Range) mod 255);
        
        if Integer(Job.Start_Most_Sig_Byte) + Most_Sig_Byte_Range - 1 > 255 then
          Job.Most_Sig_Byte_Range := 255 - Integer(Job.Start_Most_Sig_Byte);
        else
          Job.Most_Sig_Byte_Range := Most_Sig_Byte_Range - 1;
        end if;
        
        Calculated_Jobs.Append(Job);
      end;
    end loop;
  end Split_Raw_Data;
  
  
  overriding function Get_Next_Raw_Job return Rc4_Job is
    J : Rc4_Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
  overriding procedure Print_Job(The_Job : Rc4_Job; State : String) is
    From : Natural;
    To   : Natural;
  begin
    From := Integer(The_Job.Start_Most_Sig_Byte);
    To   := From + The_Job.Most_Sig_Byte_Range;
    
    Ada_Mr.Helper.Put(The_Job.Job_Id'Img, 10, 1);
    Ada_Mr.Helper.Put(From'Img, 20, 1);
    Ada_Mr.Helper.Put(" -", 3, 1);
    Ada_Mr.Helper.Put(To'Img, 5, 1);
    Ada_Mr.Helper.Put(State, 20);
    Ada.Text_IO.New_Line;
  end Print_Job;
  
  
  overriding procedure Compute_Job(The_Job : Rc4_Job) is
    Test_Key    : Rc_4.Key_Type;
    S_Box       : Rc_4.S_Box;
    X, Y        : Natural;
    Cipher_Text : Rc_4.Unsigned_Byte_Array(1 .. 10);
    Key_Found   : Boolean := false;
  begin
    for I in Natural(The_Job.Start_Most_Sig_Byte) .. (Natural(The_Job.Start_Most_Sig_Byte) + The_Job.Most_Sig_Byte_Range) loop
      Test_Key(0) := Rc_4.Unsigned_Byte(I);
      Rc_4.Print_Key(Test_Key);
      for J in Rc_4.Unsigned_Byte'Range loop
        Test_Key(1) := J;
        
        for K in Rc_4.Unsigned_Byte'Range loop
          Test_Key(2) := K;
          for L in Rc_4.Unsigned_Byte'Range loop
            Test_Key(3) := L;
            
            X := 0;
            Y := 0;
            
            Rc_4.Init_RC4(Test_Key, S_Box);
            Cipher_Text := The_Job.Plain_Text;
            
            for I in Cipher_Text'First .. Cipher_Text'Last loop
              Rc_4.Encrypt(S_Box, Cipher_Text(I), X, Y);
            end loop;
            
            if Rc_4."="(Cipher_Text, The_Job.Cipher_Text) then
              Found_Key := Test_Key;
              Key_Found := true;
            end if;
            
            exit when Key_Found = true;
          end loop;
          
          exit when Key_Found = true;
        end loop;
        
        exit when Key_Found = true;
      end loop;
      
      exit;
      exit when Key_Found = true;
    end loop;
  end Compute_Job;
  
  
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
    Mapping : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Mapping.Insert(
      "Reducer_1", 
      "<key>" & Byte_Array_To_Xml(Found_Key) & "</key>"
    );
    
    return Mapping;
  end Split_Result_For_Different_Reducer;
  
  
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean) is
    Key      : Rc_4.Key_Type;
    Null_Key : Rc_4.Key_Type := (others => 0);
  begin
    Byte_Array_From_Xml(Ada_Mr.Xml.Find_Child_With_Tag(Xml_Node, "key"), Key);
    
    if Rc_4."/="(Key, Null_Key) then
      Found_Key := Key;
      Stop_System := True;
    end if;
  end Merge_Job_Results;
  
  
  procedure Finalize is
  begin
    Ada.Text_IO.Put_Line("A possible key found:");
    Rc_4.Print_Key(Found_Key);
  end Finalize;
  
  
-- private stuff
  function Byte_Array_To_Xml(Byte_Array : Rc_4.Unsigned_Byte_Array) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    for I in Byte_Array'Range loop
      Details.Insert(Ada_Mr.Helper.Trim(I'Img), Ada_Mr.Helper.Trim(Byte_Array(I)'Img));
    end loop;
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end Byte_Array_To_Xml;
  
  
  procedure Byte_Array_From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access; Byte_Array : out Rc_4.Unsigned_Byte_Array) is
    Cursor  : Ada_Mr.Xml.Node_Access_Vector.Cursor := Xml_Node.Children.First;
  begin
    loop
      exit when Ada_Mr.Xml.Node_Access_Vector."="(Cursor, Ada_Mr.Xml.Node_Access_Vector.No_Element);
      
      Byte_Array(Integer'Value(ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Tag))) := 
        Rc_4.Unsigned_Byte(Integer'Value(ASU.To_String(Ada_Mr.Xml.Node_Access_Vector.Element(Cursor).Value)));
      
      Ada_Mr.Xml.Node_Access_Vector.Next(Cursor);
    end loop;
  end Byte_Array_From_Xml;
end Rc4_Job;