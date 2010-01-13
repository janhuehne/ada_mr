with Ada.Text_IO;

with Ada.Calendar;
with GNAT.Calendar.Time_IO;

package body Ada_Mr.Logger is
  
  procedure Set_Output_Level(Level : Log_Level) is
  begin
    Output_Level := Level;
  end Set_Output_Level;
  
  
  procedure Put(Item : String; Level : Log_Level; Prefix : String := "") is
  begin
    if Has_Correct_Level(Level) then
      if Prefix /= "" then
        Ada.Text_IO.Put("[" & Now & "][" & Level'Img & "][" & Prefix & "] " & Item);
      else
        Ada.Text_IO.Put("[" & Now & "][" & Level'Img & "] " & Item);
      end if;
    end if;
  end Put;
  
  
  procedure Put_Line(Item : String; Level : Log_Level; Prefix : String := "") is
  begin
    if Has_Correct_Level(Level) then
      if Prefix /= "" then
        Ada.Text_IO.Put_Line("[" & Now & "][" & Level'Img & "][" & Prefix & "] " & Item);
      else
        Ada.Text_IO.Put_Line("[" & Now & "][" & Level'Img & "] " & Item);
      end if;
    end if;
  end Put_Line;
  
  
  procedure New_Line(Level : Log_Level) is
  begin
    if Has_Correct_Level(Level) then
      Ada.Text_IO.New_Line;
    end if;
  end New_Line;
  
  
  function Now return String is
  begin
    return GNAT.Calendar.Time_IO.Image(Ada.Calendar.Clock, "%Y-%m-%d %H:%M:%S");
  end Now;
  
  
  function Has_Correct_Level(Level : Log_Level) return Boolean is
  begin
    case Output_Level is
      when Info =>
        return true;
      when Warn =>
        if Level = Warn OR Level = Err then
          return true;
        end if;
      when Err =>
        if Level = Err then
          return true;
        end if;
    end case;
    
    return false;
  end Has_Correct_Level;
  
  function Image(Level : Log_Level) return String is
  begin
    case Level is
      when Info => return "INFO";
      when Warn => return "WARN";
      when Err => return "ERROR";
    end case;
  end Image;
  
end Ada_Mr.Logger;