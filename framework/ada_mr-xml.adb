with Ada.Text_IO;
with Ada.Exceptions;

package body Ada_Mr.Xml is
  
  procedure Add_Node(Root_Node : in out Node_Access; Child : in out Node_Access) is
  begin
    Child.Parent := Root_Node;
    Root_Node.Children.Append(Child);
  end Add_Node;
  
  procedure Print(Root_Node : in Node_Access; Depth : Integer := 0) is
    
    procedure Iterate_Vector(c: Node_Access_Vector.Cursor) is
    begin
      Print(Node_Access_Vector.Element(c), Depth + 1);
    end Iterate_Vector;
    
  begin
    if Root_Node.Children.Is_Empty = true then
      Ada.Text_IO.Put((1 .. Depth => ' '));
      Ada.Text_IO.Put(ASU.To_String(Root_Node.Tag));
      Ada.Text_IO.Put(": ");
      Ada.Text_IO.Put(ASU.To_String(Root_Node.Value));
      Ada.Text_IO.New_Line;
    else
      Ada.Text_IO.Put((1 .. Depth => ' '));
      ada.text_io.put_Line(ASU.To_String(root_node.tag));
      Node_Access_Vector.Iterate(Root_Node.Children, Iterate_Vector'Access);
    end if;
  end Print;
  
  
  function To_String(Root_Node : in Node_Access; Depth : Integer := 0) return String is
    Result : ASU.Unbounded_String;
    
    procedure Iterate_Vector(c: Node_Access_Vector.Cursor) is
    begin
      ASU.Append(Result, To_String(Node_Access_Vector.Element(c), Depth + 1));
    end Iterate_Vector;
    
  begin
    if Root_Node.Children.Is_Empty = true then
      ASU.Append(Result, "<" & ASU.To_String(Root_Node.Tag) & ">");
      ASU.Append(Result, ASU.To_String(Root_Node.Value));
      ASU.Append(Result, "</" & ASU.To_String(Root_Node.Tag) & ">");
    else
      ASU.Append(Result, "<" & ASU.To_String(Root_Node.Tag) & ">");
      Node_Access_Vector.Iterate(Root_Node.Children, Iterate_Vector'Access);
      ASU.Append(Result, "</" & ASU.To_String(Root_Node.Tag) & ">");
    end if;
    
    return ASU.To_String(Result);
    
  end To_String;
  
  
  function Node_Content_To_String(Root_Node : in Node_Access) return String is
    Result : ASU.Unbounded_String;
    
    procedure Iterate_Vector(c: Node_Access_Vector.Cursor) is
    begin
      ASU.Append(Result, To_String(Node_Access_Vector.Element(c), 0));
    end Iterate_Vector;
    
  begin
    if Root_Node.Children.Is_Empty = true then
      ASU.Append(Result, "<" & ASU.To_String(Root_Node.Tag) & ">");
      ASU.Append(Result, ASU.To_String(Root_Node.Value));
      ASU.Append(Result, "</" & ASU.To_String(Root_Node.Tag) & ">");
    else
      Node_Access_Vector.Iterate(Root_Node.Children, Iterate_Vector'Access);
    end if;
    
    return ASU.To_String(Result);
  end Node_Content_To_String;
  
  
  function Find_Child_With_Tag(Root_Node : in Node_Access; Tag : in String) return Node_Access is
  begin
    if ASU.To_String(Root_Node.Tag) = Tag then
      return Root_Node;
    else
      declare
        Cursor : Node_Access_Vector.Cursor := Root_Node.Children.First;
      begin
        loop
          exit when not Node_Access_Vector.Has_Element(Cursor);
          
          if ASU.To_String(Node_Access_Vector.Element(Cursor).Tag) = Tag then
            return Node_Access_Vector.Element(Cursor);
          end if;
          
          Node_Access_Vector.Next(Cursor);
        end loop;
      end;
    end if;
      
    return null;
  end Find_Child_With_Tag;
  
  function Get_Value(Root : Node_Access; Tag : String) return String is
    Found_Node : Node_Access := Find_Child_With_Tag(Root, Tag);  
  begin
    return ASU.To_String(Found_Node.Value);
  exception
    when CONSTRAINT_ERROR => Ada.Exceptions.Raise_Exception(Node_Not_Found'Identity, "Tag """ & Tag & """ not found.");
  end Get_Value;
  
  function Get_Value_Or_Empty(Root : Node_Access; Tag : String) return String is
  begin
    return Get_Value(Root, Tag);
  exception
    when others => return "";
  end Get_Value_Or_Empty;
  
  
  function Get_Tag(Root : Node_Access) return String is
  begin
    return ASU.To_String(Root.Tag);
  end Get_Tag;
  
  function "="(Left, Right : Ada_Mr.Xml.Node_Access) return Boolean is
  begin
    return true;
  end "=";
end Ada_Mr.Xml;