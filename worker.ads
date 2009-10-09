with Ada.Text_IO;
with Ada.Containers.Vectors;
with Echo;

package Worker is
  
  type Worker_Type is (Mapper, Reducer, Invalid);
  
  type Worker is record
    Identifier   : String(1..10);
    W_Type       : Worker_Type;
    W_Echo       : Echo.Echo_Access;
  end record;
  
  type Worker_Access is access Worker;
  
  package Worker_Vector is new Ada.Containers.Vectors(
    Element_Type => Worker_Access,
    Index_Type => Positive
  );
  
  procedure Add_New_Worker(W_Type : String; W_Identifier : String; W_Echo : Echo.Echo_Access);
  procedure Add_New_Worker(W_Type : Worker_Type; W_Identifier : String; W_Echo : Echo.Echo_Access);

  procedure Print_Worker(C : Worker_Vector.Cursor);
  procedure To_String(W_Worker : Worker);
  
  procedure Print_All_Idle_Mapper;
  
  Invalid_Worker : exception;
    
  
--private

  
  Idle_Mapper   : Worker_Vector.Vector;
  Active_Mapper : Worker_Vector.Vector;
  
  Connected_Reducer : Worker_Vector.Vector;
  
end Worker;