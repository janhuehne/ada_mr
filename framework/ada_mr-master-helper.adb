with Ada_Mr.Helper;
with Ada_Mr.Logger;
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
  
  
  protected body Not_Delivered_Map_Results is
  
    procedure Add(Reducer : String; Result : String) is
      Tmp : Not_Delivered_Map_Result_Access;
    begin
      Tmp := new Not_Delivered_Map_Result;
      Tmp.Reducer := ASU.To_Unbounded_String(Reducer);
      Tmp.Result  := ASU.To_Unbounded_String(Result);
      
      Not_Delivered_Map_Results_Vector.Append(Tmp);
    end Add;
    
    
    function Get_All_By_Identifier(Identifier : String) return Not_Delivered_Map_Result_Vectors.Vector is
      Tmp     : Not_Delivered_Map_Result_Vectors.Vector;
      C       : Not_Delivered_Map_Result_Vectors.Cursor;
      Element : Not_Delivered_Map_Result_Access;
    begin
      C := Not_Delivered_Map_Results_Vector.First;
      
      loop
        exit when Not_Delivered_Map_Result_Vectors."="(Not_Delivered_Map_Result_Vectors.No_Element, C);
        
        Element := Not_Delivered_Map_Result_Vectors.Element(C);
        
        if ASU.To_String(Element.Reducer) = Identifier then
          Tmp.Append(Element);
          Not_Delivered_Map_Result_Vectors.Delete(
            Not_Delivered_Map_Results_Vector,
            Not_Delivered_Map_Result_Vectors.To_Index(C)
          );
        end if;
        
        Not_Delivered_Map_Result_Vectors.Next(C);
      end loop;
      
      return Tmp;
    end Get_All_By_Identifier;
    
    
    function Is_Empty return Boolean is
    begin
      return Not_Delivered_Map_Results_Vector.Is_Empty;
    end Is_Empty;
      
  end Not_Delivered_Map_Results;
  
  
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