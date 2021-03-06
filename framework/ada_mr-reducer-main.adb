with Ada.Text_IO;

with Ada_Mr.Helper;
use Ada_Mr.Helper;

with Ada_Mr.Logger;
with Ada_Mr.Reducer.Helper;

with Ada_Mr.Xml;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Xml.Parser;

with GNAT.Sockets;
with Ada.Exceptions;

package body Ada_Mr.Reducer.Main is

  ------------------
  -- Reducer_Task --
  ------------------
  task body Reducer_Task is
    Server_Task       : Server.Server.Server_Task;
    Runner_Task       : Runner.Runner.Runner_Task;
    Result_Merge_Task : Result_Merge.Runner_Task;
    
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
        accept Start
          (Self : Reducer_Task_Access) 
        do
          Main_Task := Self;
        end;
        
        
        -- reading command line arguments
        Ada_Mr.Helper.Parse_Command_Line_Arguments(Ada_Mr.Helper.Reducer);
        
        
        -- print configuration
        Ada_Mr.Helper.Print_Configuration;
        
        
        -- start local server to accept incomming connections
        Server_Task.Start(
          GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "IP")),
          GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "PORT"))
        );
        
        -- runner task to send request
        Runner_Task.Start;
        
        -- start merging task to merge mapper results
        Result_Merge_Task.Start;
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line("Stopping all reducer tasks", Ada_Mr.Logger.System);
        Ada_Mr.Reducer.Helper.Aborted.Stop;
        Server_Task.Stop;
        Runner_Task.Stop;
        Result_Merge_Task.Stop;
        
        exit;
      end select;
    end loop;
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      Ada_Mr.Reducer.Helper.Aborted.Stop;
      Server_Task.Stop;
      Runner_Task.Stop;
      Result_Merge_Task.Stop;
  end Reducer_Task;
  
  
  
  -----------------------
  -- Stop_Reducer_Task --
  -----------------------
  procedure Stop_Reducer_Task
  is
  begin
    Main_Task.Stop;
  end Stop_Reducer_Task;
  
  
  
  --------------------------
  -- Merge_Mapper_Results --
  --------------------------
  procedure Merge_Mapper_Results
  is
    Stop_Map_Reduce_System : Boolean := False;
  begin
    loop
      exit when Stop_Map_Reduce_System = True;
      exit when Ada_Mr.Reducer.Helper.Aborted.Check = true;
      
      if not Ada_Mr.Reducer.Helper.Mapper_Results.Is_Empty then
        Merge_Jobs(
          Ada_Mr.Reducer.Helper.Mapper_Results.Get_Next,
          Stop_Map_Reduce_System
        );
      end if;
    end loop;
    
    if Stop_Map_Reduce_System = True then
      Ada_Mr.Logger.Put_Line("Stopping the complete map reduce system", Ada_Mr.Logger.System);
      
      declare
        Response : String := Ada_Mr.Helper.Send(
          GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("MASTER", "IP")),
          GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("MASTER", "PORT")),
          Ada_Mr.Xml.Helper.Xml_Command(
            G_T          => Ada_Mr.Xml.Helper.Reducer,
            Command      => "stop_map_reduce_system",
            Access_Token => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN")
          ),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
        );
        
        Xml_Tree : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Helper.Get_Verified_Content(Ada_Mr.Xml.Parser.Parse(Content => Response));
      begin
        null;
      end;
      
    end if;
  end Merge_Mapper_Results;
  
  
  
  ------------
  -- Banner --
  ------------
  function Banner 
    return String 
  is
  begin
    return "ADA MR Reducer";
  end Banner;
  
end Ada_Mr.Reducer.Main;