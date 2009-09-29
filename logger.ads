package Logger is
  
  procedure Enable_Verbose_Mode;
  procedure Disable_Verbose_Mode;
    
  procedure Put_Line(Item : String);
  procedure New_Line(Item : String);

private
  
  Verbose_Mode : Boolean := false;
  
end Logger;