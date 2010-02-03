with Ada_Mr.Helper;

package body Ada_Mr.Master.Helper is

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
  
  function To_String(Arg : Job_State) return String is
  begin
    case Arg is
      when Pending => return "Pending";
      when In_Progress => return "In progress";
      when Done => return "Done";
      when Failed  => return "Failed";
    end case;
  end To_String;
  
  function From_String(Arg : String) return Job_State is
  begin
    if Ada_Mr.Helper.Is_Equal(Arg, "Pending", true) then
      return Pending;
    elsif Ada_Mr.Helper.Is_Equal(Arg, "In_Progress", true) then
      return In_Progress;
    elsif Ada_Mr.Helper.Is_Equal(Arg, "Done", true) then
      return Done;
    elsif Ada_Mr.Helper.Is_Equal(Arg, "Failed", true) then
      return Failed;
    else
      raise Unknown_Job_State;
    end if;
  end From_String;

end Ada_Mr.Master.Helper;