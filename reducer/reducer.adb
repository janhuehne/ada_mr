with Ada.Text_IO;

with Utility;
use Utility;

with Logger;
with Reducer_Helper;

with GNAT.Sockets;

package body Reducer is


----------------------------------------------------
-- REDUCER TASK                                   -
----------------------------------------------------
  task body Reducer_Task is
    Server_Task       : Server.Server.Server_Task;
    Result_Merge      : Result_Merge_Task;
  begin
    loop
      select
        accept Start;
        Server_Task.Start(Reducer_Helper.Server_Bind_Ip, Reducer_Helper.Server_Bind_Port);
        Result_Merge.Start;
      or
        accept Stop;
        Server_Task.Stop;
--        Reducer_Helper.Aborted.Stop;
        exit;
      end select;
    end loop;
    null;
  end Reducer_Task;
  
  
  
----------------------------------------------------
-- RESULT MERGE TASK                               -
----------------------------------------------------
  task body Result_Merge_Task is
  begin
    accept Start;
    Ada.Text_IO.Put_Line("Result Merge Task started!");
    loop
      exit when Reducer_Helper.Aborted.Check = true;
      
      declare
        Cursor : Reducer_Helper.Xml_Node_Access_Vectors.Cursor := Reducer_Helper.Finished_Jobs_Queue.First;
      begin
        loop
          exit when Reducer_Helper.Xml_Node_Access_Vectors."="(Cursor, Reducer_Helper.Xml_Node_Access_Vectors.No_Element);
          
          declare
          begin
            Merge_Jobs(Reducer_Helper.Xml_Node_Access_Vectors.Element(Cursor));
            Reducer_Helper.Finished_Jobs_Queue.Delete(Cursor);
          exception
            when Error : others => Utility.Print_Exception(Error);
          end;
          
          Reducer_Helper.Xml_Node_Access_Vectors.Next(Cursor);
        end loop;
      end;
    end loop;
  end Result_Merge_Task;
  
  
  
----------------------------------------------------
-- GENERIC CONSOLE INSTANCE                       --
----------------------------------------------------
  function Banner return String is
  begin
    return "ADA MR Reducer";
  end Banner;
  
  
  procedure Parse_Configuration(Config_Xml : Xml.Node_Access) is
  begin
    
    Reducer_Helper.Identifier := ASU.To_Unbounded_String(Xml.Get_Value(Config_Xml, "identifier"));
    
    declare
      Local_Server_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "local_server");
    begin
      Reducer_Helper.Server_Bind_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Local_Server_Details, "bind_ip"));
      Reducer_Helper.Server_Bind_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Local_Server_Details, "bind_port"));
    end;
    
--    declare
--      Master_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "master");
--    begin
--      Reducer_Helper.Master_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Master_Details, "ip"));
--      Reducer_Helper.Master_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Master_Details, "port"));
--    end;
    
  end Parse_Configuration;
  
  
  procedure Process_User_Input(User_Input : String; To_Controll : Reducer_Task_Access) is
  begin
    if (Is_Equal(User_Input, "config", true)) then
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("-> " & Banner & " configuration");
      
      Utility.Put("Identifier:", 20, 2);
      Utility.Put(ASU.To_String(Reducer_Helper.Identifier), 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Access token:", 20, 2);
      Utility.Put(Reducer_Helper.Access_Token, 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Listen on ip:", 20, 2);
      Utility.Put(GNAT.Sockets.Image(Reducer_Helper.Server_Bind_Ip), 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Listen on port:", 20, 2);
      Utility.Put(Reducer_Helper.Server_Bind_Port'Img, 60, 2);
      Ada.Text_IO.New_Line;
      
    elsif (Is_Equal(User_Input, "start", true)) then
      To_Controll.Start;
    
    elsif (Is_Equal(User_Input, "quit", true)) or (Is_Equal(User_Input, "exit", true)) then
      To_Controll.Stop;
    
    elsif (Is_Equal(User_Input, "help", true)) then
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("  Commands:");
      Ada.Text_IO.Put_Line("    start        Starts the Ada MR Reducer");
      Ada.Text_IO.Put_Line("    abort        Stops the Ada MR Reducer immediately (current job is aborted)");
      Ada.Text_IO.Put_Line("    quit / exit  Stops the Ada MR Reducer after processing the last job");
    
    else
      Ada.Text_IO.Put_Line("Unknown command: " & User_Input);
    end if;
    
  end Process_User_Input;
  
end Reducer;