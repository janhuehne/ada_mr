with Ada.Text_IO;

with Ada_Mr.Helper;
use Ada_Mr.Helper;

with Ada_Mr.Logger;
with Ada_Mr.Reducer.Helper;

with GNAT.Sockets;
with Ada.Exceptions;

package body Ada_Mr.Reducer.Main is

----------------------------------------------------
-- REDUCER TASK                                   -
----------------------------------------------------
  task body Reducer_Task is
    Server_Task       : Server.Server.Server_Task;
    Runner_Task       : Runner.Runner.Runner_Task;
    Result_Merge      : Result_Merge_Task;
    
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
        accept Start(Self : Reducer_Task_Access) do
          Main_Task := Self;
        end;
        
        
        -- set default configuration
        Ada_Mr.Helper.Set_Default_Configuration(Ada_Mr.Helper.Mapper);
        
        
        -- reading command line arguments
        Ada_Mr.Helper.Parse_Command_Line_Arguments(Ada_Mr.Helper.Mapper);
        
        
        -- print configuration
        Ada_Mr.Helper.Print_Configuration;
        
        
        -- start local server to accept incomming connections
        Server_Task.Start(
          GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER-BIND_IP")),
          GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER-BIND_PORT"))
        );
        
        -- runner task to send request
        Runner_Task.Start;
        
        -- start merging task to merge mapper results
        Result_Merge.Start;
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line("Stopping all reducer tasks", Ada_Mr.Logger.Info);
        Ada_Mr.Reducer.Helper.Aborted.Stop;
        Server_Task.Stop;
        Runner_Task.Stop;
        Result_Merge.Stop;
        
        exit;
      end select;
    end loop;
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      Ada_Mr.Reducer.Helper.Aborted.Stop;
      Server_Task.Stop;
      Runner_Task.Stop;
      Result_Merge.Stop;
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
    loop
      select
        accept Start;
        Ada_Mr.Logger.Put_Line("Result Merge Task started!", Ada_Mr.Logger.Info);
        loop
          exit when Ada_Mr.Reducer.Helper.Aborted.Check = true;
          
          declare
            Cursor : Ada_Mr.Reducer.Helper.Xml_Node_Access_Vectors.Cursor := Ada_Mr.Reducer.Helper.Finished_Jobs_Queue.First;
          begin
            loop
              exit when Ada_Mr.Reducer.Helper.Xml_Node_Access_Vectors."="(Cursor, Ada_Mr.Reducer.Helper.Xml_Node_Access_Vectors.No_Element);
              
              declare
              begin
                Merge_Jobs(Ada_Mr.Reducer.Helper.Xml_Node_Access_Vectors.Element(Cursor));
                Ada_Mr.Reducer.Helper.Finished_Jobs_Queue.Delete(Cursor);
              exception
                when Error : others => Ada_Mr.Helper.Print_Exception(Error);
              end;
              
              Ada_Mr.Reducer.Helper.Xml_Node_Access_Vectors.Next(Cursor);
            end loop;
          end;
        end loop;
      or 
        accept Stop;
        Ada_Mr.Logger.Put_Line("Terminating result merge task", Ada_Mr.Logger.Info);
        exit;
      end select;
    end loop;
    Ada_Mr.Logger.Put_Line("Result merge task terminated", Ada_Mr.Logger.Info);
  end Result_Merge_Task;
  
  
  
----------------------------------------------------
-- GENERIC CONSOLE INSTANCE                       --
----------------------------------------------------
  function Banner return String is
  begin
    return "ADA MR Reducer";
  end Banner;
  
end Ada_Mr.Reducer.Main;