package Ada_Mr.Logger is
  
  type Log_Level is (Info, Warn, Err);
  
  procedure Set_Output_Level(Level : Log_Level);
  
  procedure Put(Item : String; Level : Log_Level; Prefix : String := "");
  procedure Put_Line(Item : String; Level : Log_Level; Prefix : String := "");
  procedure New_Line(Level : Log_Level);
  
  function Image(Level : Log_Level) return String;
  
private
  function Now return String;
  function Has_Correct_Level(Level : Log_Level) return Boolean;
  
  Output_Level : Log_Level := Info;
end Ada_Mr.Logger;