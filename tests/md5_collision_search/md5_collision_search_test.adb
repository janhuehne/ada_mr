with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with GNAT.MD5;

procedure MD5_Collision_Search_Test is
  package ASU renames Ada.Strings.Unbounded;
  
  File             : Ada.Text_IO.File_Type;
  File_Name        : ASU.Unbounded_String;
  Collision_Length : Natural := 32;
  
  
  Row    : String (1 .. 999);
  Length : Natural;
  
  MD5_String_1, Tmp_MD5_String_1 : String(1 .. 32);
  MD5_String_2, Tmp_MD5_String_2 : String(1 .. 32);
  MD5_String_Pos : Natural := 0;
  
  Null_String    : String(1 .. 32) := "00000000000000000000000000000000";
  
  Test_Okay : Boolean := False;
begin
  File_Name        := ASU.To_Unbounded_String(Ada.Command_Line.Argument(1));
  Collision_Length := Integer'Value(Ada.Command_Line.Argument(2));
  
  -- Try to open the file
  begin
    Ada.Text_IO.Open(
      File => File,
      Mode => Ada.Text_IO.In_File,
      Name => ASU.To_String(File_Name));
  exception
    when others =>
      Ada.Text_IO.Put_Line("Could not open file.");
      return;
  end;
  
  -- Read file
  begin
    loop
      Ada.Text_IO.Get_Line(
        File => File,
        Item => Row,
        Last => Length
      );
      
      if Row(1..4) = "MD5(" then
        case MD5_String_Pos is
          when 0 => 
            MD5_String_1   := Row(5..5+32-1);
            MD5_String_Pos := 1;
          when 1 =>
            MD5_String_2   := Row(5..5+32-1);
            MD5_String_Pos := 2;
          when others => exit;
        end case;
      end if;
    end loop;
  exception
    when Ada.IO_Exceptions.End_Error => null;
  end;
  
  Tmp_MD5_String_1 := GNAT.MD5.Digest(MD5_String_1);
  Tmp_MD5_String_2 := GNAT.MD5.Digest(MD5_String_2);
  
  Tmp_MD5_String_1(1 .. 32 - Collision_Length) := Null_String(1 .. 32 - Collision_Length);
  Tmp_MD5_String_2(1 .. 32 - Collision_Length) := Null_String(1 .. 32 - Collision_Length);
  
  Ada.Text_IO.Close(File);
  
  if Tmp_MD5_String_1 = Tmp_MD5_String_2 then
    Test_Okay := True;
  end if;
  
  
  if Test_Okay = True then
    Ada.Text_IO.Put_Line("Collision found!");
    Ada.Text_IO.Put_Line("MD5(" & MD5_String_1 & ") = " & Tmp_MD5_String_1);
    Ada.Text_IO.Put_Line("MD5(" & MD5_String_2 & ") = " & Tmp_MD5_String_2);
  else
    Ada.Text_IO.Put_Line("There is a problem! Test failed!");
  end if;
  
end MD5_Collision_Search_Test;