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
      
      Vector.Append(Tmp);
    end Add;
    
    
    function Get_All_Pending_By_Identifier(Identifier : String) return Not_Delivered_Map_Result_Vectors.Vector is
      Tmp     : Not_Delivered_Map_Result_Vectors.Vector;
      C       : Not_Delivered_Map_Result_Vectors.Cursor;
      Element : Not_Delivered_Map_Result_Access;
    begin
      C := Vector.First;
      
      loop
        exit when Not_Delivered_Map_Result_Vectors."="(Not_Delivered_Map_Result_Vectors.No_Element, C);
        
        Element := Not_Delivered_Map_Result_Vectors.Element(C);
        
        if ASU.To_String(Element.Reducer) = Identifier and Element.Pending = True then
          Tmp.Append(Element);
          Element.Pending := False;
        end if;
        
        Not_Delivered_Map_Result_Vectors.Next(C);
      end loop;
      
      return Tmp;
    end Get_All_Pending_By_Identifier;
    
    
    function All_Done return Boolean is
      C : Not_Delivered_Map_Result_Vectors.Cursor;
    begin
      loop
        exit when Not_Delivered_Map_Result_Vectors."="(Not_Delivered_Map_Result_Vectors.No_Element, C);
        
        if Not_Delivered_Map_Result_Vectors.Element(C).Pending = True then
          return False;
        end if;
        
        Not_Delivered_Map_Result_Vectors.Next(C);
      end loop;
      
      return True;
    end All_Done;
      
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