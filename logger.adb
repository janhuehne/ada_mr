with Ada.Text_IO;

package body Logger is
  
  procedure Enable_Verbose_Mode is
  begin
    Verbose_Mode := true;
  end Enable_Verbose_Mode;
  
  procedure Disable_Verbose_Mode is
  begin
    Verbose_Mode := false;
  end Disable_Verbose_Mode;
  
  procedure Put_Line(Item : String) is
  begin
    Ada.Text_IO.Put_Line(Item);
  end Put_Line;
  
  procedure New_Line(Item : String) is
  begin
    Ada.Text_IO.New_Line;
  end New_Line;
  
end Logger;