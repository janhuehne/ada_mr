with Ada.Text_IO;

with Application_Helper;
use Application_Helper;

with Logger;
with Reducer_Helper;

with GNAT.Sockets;
with Xml_Parser;
with Ada.Exceptions;

package body Reducer is

----------------------------------------------------
-- REDUCER TASK                                   -
----------------------------------------------------
  task body Reducer_Task is
    Server_Task       : Server.Server.Server_Task;
    Runner_Task       : Runner.Runner.Runner_Task;
    Result_Merge      : Result_Merge_Task;
    
    procedure Read_and_Parse_Config_File(Config_File : String) is
    begin
      if Application_Helper.Does_File_Exist(Config_File) then
        Ada.Text_IO.Put_Line("Parsing config file");
        Parse_Configuration(
          Xml_Parser.Parse(File_Name => Config_File)
        );
        Ada.Text_IO.Put_Line("--> Done");
      else
        Ada.Text_IO.Put_Line("No config file found!");
      end if;
    exception
      when Error : others => 
        Application_Helper.Print_Exception(Error);
        Ada.Exceptions.Raise_Exception(Application_Helper.Configuration_File_Error'Identity, "There is a problem with the configuration file.");
    end Read_and_Parse_Config_File;
    
    
    procedure Print_Configuration is
    begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line(Banner & " configuration");
            
      Application_Helper.Put("--> Local IP address:", 30, 2);
      Application_Helper.Put(GNAT.Sockets.Image(Reducer_Helper.Server_Bind_Ip), 60, 2);
      Ada.Text_IO.New_Line;
      
      Application_Helper.Put("--> Local port:", 30, 2);
      Application_Helper.Put(Reducer_Helper.Server_Bind_Port'Img, 60, 2);
      Ada.Text_IO.New_Line;
      
      Application_Helper.Put("--> Master host:", 30, 2);
      Application_Helper.Put(GNAT.Sockets.Image(Reducer_Helper.Master_Ip), 60, 2);
      Ada.Text_IO.New_Line;
      
      Application_Helper.Put("--> Master port:", 30, 2);
      Application_Helper.Put(Reducer_Helper.Master_Port'Img, 60, 2);
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Print_Configuration;
    
  begin
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    
    Ada.Text_IO.Put_Line("          _____               __  __ _____      _____          _                     ");
    Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  __ \        | |                    ");
    Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | |__) |___  __| |_   _  ___ ___ _ __ ");
    Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    |  _  // _ \/ _` | | | |/ __/ _ \ '__|");
    Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | | \ \  __/ (_| | |_| | (__  __/ |   ");
    Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  \_\___|\__,_|\__,_|\___\___|_|   ");
    
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    
    
    loop
      select
        accept Start(Self : Reducer_Task_Access; Config_File : String) do
          Main_Task := Self;
          
          -- parse configuration
          Read_and_Parse_Config_File(Config_File);
        end;
        
        -- print configuration
        Print_Configuration;
        
        -- start local server to accept incomming connections
        Server_Task.Start(Reducer_Helper.Server_Bind_Ip, Reducer_Helper.Server_Bind_Port);
        
        -- runner task to send request
        Runner_Task.Start;
        
        -- start merging task to merge mapper results
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
  
  
  procedure Stop_Reducer_Task is
  begin
    Main_Task.Stop;
  end Stop_Reducer_Task;
  
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
            when Error : others => Application_Helper.Print_Exception(Error);
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
    
    declare
      Master_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "master");
    begin
      Reducer_Helper.Master_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Master_Details, "ip"));
      Reducer_Helper.Master_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Master_Details, "port"));
    end;
    
--    declare
--      Master_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "master");
--    begin
--      Reducer_Helper.Master_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Master_Details, "ip"));
--      Reducer_Helper.Master_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Master_Details, "port"));
--    end;
    
  end Parse_Configuration;
  
end Reducer;