with Ada_Mr.Helper;
with Ada_Mr.Logger;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Xml.Parser;
package body Ada_Mr.Reducer.Helper is
  
  -------------
  -- Aborted --
  -------------
  protected body Aborted
  is
    procedure Stop 
    is
    begin
      Abort_It := true;
    end Stop;
    
    function Check
      return Boolean
    is
    begin
      return Abort_It;
    end Check;
  end Aborted;
  
  
  
  -----------------------------------------------------
  -- Import_Not_Delivered_Mapper_Results_From_Master --
  -----------------------------------------------------
  procedure Import_Not_Delivered_Mapper_Results_From_Master is
    Response : String := Ada_Mr.Helper.Send(
      GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("MASTER-IP")),
      GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("MASTER-PORT")),
      Ada_Mr.Xml.Helper.Xml_Command(
        G_T     => Ada_Mr.Xml.Helper.Reducer,
        Command => "get_pending_map_results",
        Access_Token => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN")
      ),
      Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
      Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
    );
    
    Xml_Tree         : Ada_Mr.Xml.Node_Access;
    Xml_Details_Tree : Ada_Mr.Xml.Node_Access;
    
    procedure Import_Mapper_Results(
      C : Ada_Mr.Xml.Node_Access_Vector.Cursor)
    is
    begin
      Ada_Mr.Logger.Info("Importing mapper result from the master.");
      
      Mapper_Results.Add(
        Ada_Mr.Xml.Node_Access_Vector.Element(C)
      );
    end Import_Mapper_Results;
    
  begin
    Xml_Tree := Ada_Mr.Xml.Helper.Get_Verified_Content(Ada_Mr.Xml.Parser.Parse(Content => Response));
    
    if Ada_Mr.Xml.Helper.Is_Command(Xml_Tree, "pending_results") then
      Xml_Details_Tree := Ada_Mr.Xml.Find_Child_With_Tag(Xml_Tree, "details");
      
      Xml_Details_Tree.Children.Iterate(Import_Mapper_Results'Access);
    end if;
  end Import_Not_Delivered_Mapper_Results_From_Master;
  
  
  
  --------------------
  -- Mapper_Results --
  --------------------
  protected body Mapper_Results is
  
    procedure Add
      (Node : Ada_Mr.Xml.Node_Access) 
    is
    begin
      Mapper_Results_Vector.Append(Node);
    end Add;
    
    procedure Add
      (Node_Vector : Ada_Mr.Xml.Node_Access_Vector.Vector)
    is
    begin
      Mapper_Results_Vector.Append(Node_Vector);
    end Add;
    
    function Is_Empty
      return Boolean 
    is
    begin
      return Mapper_Results_Vector.Is_Empty;
    end Is_Empty;
    
    function Get_Next
      return Ada_Mr.Xml.Node_Access
    is
      Tmp : Ada_Mr.Xml.Node_Access;
    begin
      Tmp := Mapper_Results_Vector.First_Element;
      Mapper_Results_Vector.Delete_First;
      
      return Tmp;
    end Get_Next;
    
  end Mapper_Results;
  
end Ada_Mr.Reducer.Helper;