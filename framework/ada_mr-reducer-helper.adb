package body Ada_Mr.Reducer.Helper is
  
  protected body Aborted is
  
    procedure Stop is
    begin
      Abort_It := true;
    end Stop;
    
    function Check return Boolean is
    begin
      return Abort_It;
    end Check;
    
  end Aborted;
  
end Ada_Mr.Reducer.Helper;