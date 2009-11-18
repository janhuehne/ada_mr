package body Master_Helper is

  protected body Aborted is
  
    procedure Stop_Master is
    begin
      Abort_Master := true;
    end Stop_Master;
    
    
    procedure Stop_Clients is
    begin
      Abort_Clients := true;
    end Stop_Clients;
    
    function Check_Master return Boolean is
    begin
      return Abort_Master;
    end Check_Master;
    
    function Check_Clients return Boolean is
    begin
      return Abort_Clients;
    end Check_Clients;
    
  end Aborted;
  
  
  function String_To_Worker_Type(Arg : String) return Worker_Type is
  begin
    if Arg = "Mapper" then
      return Mapper;
    elsif Arg = "Reducer" then
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

end Master_Helper;