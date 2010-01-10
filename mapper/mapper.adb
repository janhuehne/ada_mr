with Ada.Text_IO;

with Application_Helper;
use Application_Helper;

with Logger;
with Mapper_Helper;

with Xml_Helper;
with Ada.Exceptions;

with Xml_Parser;

package body Mapper is
  
----------------------------------------------------
-- MAPPER TASK                                    --
----------------------------------------------------
  task body Mapper_Task is
    Runner_Task   : Runner.Runner.Runner_Task;
    Server_Task   : Server.Server.Server_Task;
    
  begin
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("          _____               __  __ _____      __  __                             ");
    Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  \/  |                            ");
    Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | \  / | __ _ _ __  _ __   ___ _ __ ");
    Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    | |\/| |/ _` | '_ \| '_ \ / _ \ '__|");
    Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | |  | | (_| | |_) | |_) |  __/ |   ");
    Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  |_|\__,_| .__/| .__/ \___|_|   ");
    Ada.Text_IO.Put_Line("                                                            | |   | |              ");
    Ada.Text_IO.Put_Line("                                                            |_|   |_|               ");

    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    
    loop
      select
        accept Start(Self : Mapper_Task_Access; Config_File : String) do
          Main_Task := Self;
          
          -- parse configuration
          --Read_and_Parse_Config_File(Config_File);
          Application_Helper.Set_Default_Configuration(Application_Helper.Mapper);
          Application_Helper.Parse_Configuration(Config_File, Application_Helper.Mapper);
        end Start;
        
        -- print configuration
        Application_Helper.Print_Configuration;
        
        -- start local server to accept incomming connections
        Server_Task.Start(
          GNAT.Sockets.Inet_Addr(Application_Helper.Read_Configuration("LOCAL_SERVER-BIND_IP")),
          GNAT.Sockets.Port_Type'Value(Application_Helper.Read_Configuration("LOCAL_SERVER-BIND_PORT"))
        );
        
        -- runner task 
        Runner_Task.Start;
      or
        accept Stop;
        Logger.Put_Line("Depending tasks will be terminted", Logger.Info);
        Mapper_Helper.Aborted.Stop;
        Runner_Task.Stop;
        Server_Task.Stop;
        exit;
      end select;
    end loop;
  exception
    when Error : others =>
      Application_Helper.Print_Exception(Error);
      Mapper_Helper.Aborted.Stop;
      Runner_Task.Stop;
      Server_Task.Stop;
  end Mapper_Task;
  
  
  procedure Stop_Mapper_Task is
  begin
    Main_Task.Stop;
  end Stop_Mapper_Task;
  
----------------------------------------------------
-- GENERIC CONSOLE METHODS                        --
----------------------------------------------------
  function Banner return String is
  begin
    return "ADA MR Mapper";
  end Banner;
  
end Mapper;