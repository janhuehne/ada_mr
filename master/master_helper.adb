with Utility;

package body Master_Helper is

  protected body Aborted is
    
    procedure Set_Abort is
    begin
      Abort_Master := true;
    end Set_Abort;
    
    procedure Set_Exit is
    begin
      Exit_Master := true;
    end Set_Exit;
    
    function Get_Abort return Boolean is
    begin
      return Abort_Master;
    end Get_Abort;
    
    function Get_Exit return Boolean is
    begin
      return Exit_Master;
    end Get_Exit;
    
  end Aborted;
  
  
  function String_To_Worker_Type(Arg : String) return Worker_Type is
  begin
    if Utility.Is_Equal(Arg, "Mapper", true) then
      return Mapper;
    elsif Utility.Is_Equal(Arg, "Reducer", true) then
      return Reducer;
    else
      raise Unknow_Worker_Type;
    end if;
  end String_To_Worker_Type;
  
  
  function To_String(Arg : Worker_Type) return String is
  begin
    case Arg is
      when Mapper => return "Mapper";
      when Reducer => return "Reducer";
      when Invalid => return "N/A";
    end case;
  end To_String;
  
  function To_String(Arg : Job_State) return String is
  begin
    case Arg is
      when Pending => return "Pending";
      when In_Progress => return "In progress";
      when Done => return "Done";
    end case;
  end To_String;
  
  function From_String(Arg : String) return Job_State is
  begin
    if Utility.Is_Equal(Arg, "Pending", true) then
      return Pending;
    elsif Utility.Is_Equal(Arg, "In_Progress", true) then
      return In_Progress;
    elsif Utility.Is_Equal(Arg, "Done", true) then
      return Done;
    else
      raise Unknown_Job_State;
    end if;
  end From_String;

end Master_Helper;