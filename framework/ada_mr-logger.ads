package Ada_Mr.Logger is
  
  type Log_Level is (Info, Warn, Err, User, System, Debug);
  
  procedure Set_Output_Level(Level : Log_Level);
  procedure Set_Output_Level(Level : String);
  
  procedure Put(Item : String; Level : Log_Level; Prefix : String := "");
  procedure Put_Line(Item : String; Level : Log_Level; Prefix : String := "");
  procedure New_Line(Level : Log_Level);
  
  procedure Info(Item : String; Prefix : String := "");
  procedure Warn(Item : String; Prefix : String := "");
  procedure Error(Item : String; Prefix : String := "");
  procedure User(Item : String; Prefix : String := "");
  procedure System(Item : String; Prefix : String := "");
  procedure Debug(Item : String; Prefix : String := "");
  
  function Image(Level : Log_Level) return String;
  function From_String(Level : String) return Log_Level;
  
private
  function Now return String;
  function Has_Correct_Level(Level : Log_Level) return Boolean;
  
  Output_Level : Log_Level := Info;
end Ada_Mr.Logger;