package body Mapper_Helper is
  
  protected body Aborted is
    
    procedure Set_Abort is
    begin
      Abort_Mapper := true;
    end Set_Abort;
    
    procedure Set_Exit is
    begin
      Exit_Mapper := true;
    end Set_Exit;
    
    function Get_Abort return Boolean is
    begin
      return Abort_Mapper;
    end Get_Abort;
    
    function Get_Exit return Boolean is
    begin
      return Exit_Mapper;
    end Get_Exit;
    
  end Aborted;
  
  
end Mapper_Helper;