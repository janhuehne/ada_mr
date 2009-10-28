with Ada.Text_IO;
with Utility;

package body Xml_Queue is
  
  procedure Add_Job(Job_Id : Natural; Xml_Representation : String; State : Job_State := Pending) is
    Job : Xml_Job_Entry_Access := new Xml_Job_Entry;
  begin
    Job.Id    := Job_Id;
    Job.Xml   := ASU.To_Unbounded_String(Xml_Representation);
    Job.State := State;
    
    Jobs.Append(Job);
  end Add_Job;
  
--  function Get_Next_Job(Remove_From_Collection : Boolean := true) return Xml_Job_Entry is
--    Cursor : Unbounded_String_Vector.Cursor := Unprocessed_Jobs.First;
--    Job    : Xml_Job_Entry                  := Unprocessed_Jobs.Element(Unbounded_String_Vector.To_Index(Cursor));
--  begin
--    if Remove_From_Collection = true then
--      Unprocessed_Jobs.Delete(Unbounded_String_Vector.To_Index(Cursor));
--    end if;
--    
--    return Job;
--  end Get_Next_Job;
  
  function "="(Left, Right : Xml_Job_Entry_Access) return Boolean is
  begin
    return true;
  end "=";
  
  
  function Count_Jobs(State : Job_State) return Natural is
    Counter : Natural := 0;
    
    procedure Count(Position : Unbounded_String_Vector.Cursor) is
    begin
      if Jobs.Element(Unbounded_String_Vector.To_Index(Position)).State = State then
        Counter := Counter + 1;
      end if;
    end Count;
    
  begin
    Jobs.Iterate(Count'Access);
    
    return Counter;
  end Count_Jobs;
  
  
  function Find_Job_By_Id(Job_Id : Natural) return Xml_Job_Entry_Access is
    Cursor : Unbounded_String_Vector.Cursor := Jobs.First;
  begin
    loop
      exit when Unbounded_String_Vector."="(Cursor, Unbounded_String_Vector.No_Element);
      
      if Jobs.Element(Unbounded_String_Vector.To_Index(Cursor)).Id = Job_Id then
        return Jobs.Element(Unbounded_String_Vector.To_Index(Cursor));
      end if;
      
      Unbounded_String_Vector.Next(Cursor);
    end loop;
      
    raise Job_Not_Found;
  end Find_Job_By_Id;
  
  
  function Find_First_Job_By_State(State : Job_State) return Xml_Job_Entry_Access is
    Cursor : Unbounded_String_Vector.Cursor := Jobs.First;
  begin
    loop
      exit when Unbounded_String_Vector."="(Cursor, Unbounded_String_Vector.No_Element);
      
      if Jobs.Element(Unbounded_String_Vector.To_Index(Cursor)).State = State then
        return Jobs.Element(Unbounded_String_Vector.To_Index(Cursor));
      end if;
      
      Unbounded_String_Vector.Next(Cursor);
    end loop;
      
    raise Job_Not_Found;
  end Find_First_Job_By_State;
  
  
  procedure Change_Job_State(Job_Id : Natural; New_State : Job_State) is
    Job : Xml_Job_Entry_Access := Find_Job_By_Id(Job_Id);
  begin
    Job.State := New_State;
  end Change_Job_State;
  
  
  procedure Change_Job_State(Job : Xml_Job_Entry_Access; New_State : Job_State) is
  begin
    Job.State := New_State;
  end Change_Job_State;
  
  
  procedure Change_Job_State(Job_Id : String; New_State : String) is
    State : Job_State;
  begin
    null;
    if Utility.Is_Equal(New_State, "Pending", true) then
      State := Pending;
    elsif Utility.Is_Equal(New_State, "In_Progress", true) then
      State := In_Progress;
    elsif Utility.Is_Equal(New_State, "Done", true) then
      State := Done;
    else
      raise Invalid_Job_State;
    end if;
    
    Change_Job_State(Integer'Value(Job_Id), State);
  end Change_Job_State;
  
  procedure Print_Jobs is
    
    procedure Print(Position : Unbounded_String_Vector.Cursor) is
      Job : Xml_Job_Entry_Access := Jobs.Element(Unbounded_String_Vector.To_Index(Position));
    begin
      Ada.Text_IO.Put_Line("     " & Job.Id'Img & "          " & To_String(Job.State));
    end Print;
    
  begin
    Ada.Text_IO.Put_Line("     ID          Status    ");
    Ada.Text_IO.Put_Line("     ----------------------");
    Jobs.Iterate(Print'Access);
  end Print_Jobs;
  
  
  function To_String(State : Job_State) return String is
  begin
    case State is
      when Pending => return "Pending";
      when In_Progress => return "In progress";
      when Done => return "Done";
    end case;
  end To_String;
  
  
  function Get_Job_State(Job_Id : Integer) return String is
  begin
    return To_String(Find_Job_By_Id(Job_Id).State);
  end Get_Job_State;
--  function Get_NextAuto_Inc_Value return Positive is
--  begin
--    Auto_Inc_Counter := Auto_Inc_Counter + 1;
--    return Auto_Inc_Counter;
--  end
  
end Xml_Queue;