with Ada.Text_IO;

with Application_Helper;
use Application_Helper;

with Logger;
with Reducer_Helper;

with GNAT.Sockets;
with Ada.Exceptions;

package body Reducer is

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
        accept Start(Self : Reducer_Task_Access; Config_File : String) do
          Main_Task := Self;
          
          -- parse configuration
          Application_Helper.Set_Default_Configuration(Application_Helper.Reducer);
          Application_Helper.Parse_Configuration(Config_File, Application_Helper.Reducer);
        end;
        
        -- print configuration
        Application_Helper.Print_Configuration;
        
        -- start local server to accept incomming connections
        Server_Task.Start(
          GNAT.Sockets.Inet_Addr(Application_Helper.Read_Configuration("LOCAL_SERVER-BIND_IP")),
          GNAT.Sockets.Port_Type'Value(Application_Helper.Read_Configuration("LOCAL_SERVER-BIND_PORT"))
        );
        
        -- runner task to send request
        Runner_Task.Start;
        
        -- start merging task to merge mapper results
        Result_Merge.Start;
      or
        accept Stop;
        Logger.Put_Line("Stopping all reducer tasks", Logger.Info);
        Reducer_Helper.Aborted.Stop;
        Server_Task.Stop;
        Runner_Task.Stop;
        Result_Merge.Stop;
        
        exit;
      end select;
    end loop;
  exception
    when Error : others =>
      Application_Helper.Print_Exception(Error);
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
        Logger.Put_Line("Result Merge Task started!", Logger.Info);
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
      or 
        accept Stop;
        Logger.Put_Line("Terminating result merge task", Logger.Info);
        exit;
      end select;
    end loop;
    Logger.Put_Line("Result merge task terminated", Logger.Info);
  end Result_Merge_Task;
  
  
  
----------------------------------------------------
-- GENERIC CONSOLE INSTANCE                       --
----------------------------------------------------
  function Banner return String is
  begin
    return "ADA MR Reducer";
  end Banner;
  
end Reducer;