with Utility;

package body Worker is
  
  procedure Add_New_Worker(W_Type : String; W_Identifier : String; W_Echo : Echo.Echo_Access) is
    Worker : Worker_Type;
  begin
    if W_Type = "Mapper" then
      Worker := Mapper;
    elsif W_Type = "Reducer" then
      Worker := Reducer;
    else
      Worker := Invalid;
    end if;
    
    Add_New_Worker(Worker, W_Identifier, W_Echo);
    
  end Add_New_Worker;
  
  
  procedure Add_New_Worker(W_Type : Worker_Type; W_Identifier : String; W_Echo : Echo.Echo_Access) is
    W_Worker : Worker_Access;
  begin
    if W_Type = Reducer or W_Type = Mapper then
      W_Worker := new Worker;
      W_Worker.Identifier(W_Identifier'First .. W_Identifier'Last) := W_Identifier;
      W_Worker.W_Type := W_Type;
      W_Worker.W_Echo := W_Echo;
    end if;
    
    if W_Type = Mapper then
      Idle_Mapper.Append(W_Worker);
    elsif W_Type = Reducer then
      Connected_Reducer.Append(W_Worker);
    else
      raise Invalid_Worker;
    end if;
  end Add_New_Worker;
  
  
  procedure Print_Worker(C : Worker_Vector.Cursor) is
  begin
    To_String(Worker_Vector.Element(C).all);
  end Print_Worker;
  
  
  procedure To_String(W_Worker : Worker) is
  begin
    Ada.Text_IO.Put("     ");
    Ada.Text_IO.Put(W_Worker.Identifier);
    Ada.Text_IO.New_Line;
  end To_String;
  
  
  procedure Print_All_Idle_Mapper is
  begin
    Ada.Text_IO.New_Line;
    if Idle_Mapper.Is_Empty then
      Ada.Text_IO.Put_Line("   There are no idle mappers.");
    else
      Ada.Text_IO.Put_Line("   The following mappers are idle");
      Ada.Text_IO.Put_Line("   ******************************");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("     Identifier");
      Ada.Text_IO.Put_Line("     ------------------------------------");
      Idle_Mapper.Iterate(Print_Worker'Access);
    end if;
      
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
  end Print_All_Idle_Mapper;
  
end Worker;