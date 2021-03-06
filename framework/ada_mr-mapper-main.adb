with Ada.Text_IO;
with Ada.Exceptions;

with Ada_Mr.Helper;
use Ada_Mr.Helper;

with Ada_Mr.Logger;
with Ada_Mr.Mapper.Helper;

with Ada_Mr.Xml.Helper;
with Ada_Mr.Xml.Parser;

with Ada.Command_Line;

package body Ada_Mr.Mapper.Main is
  
  -----------------
  -- Mapper_Task --
  -----------------
  task body Mapper_Task is
    Runner_Task   : Runner.Runner.Runner_Task;
    Server_Task   : Server.Server.Server_Task;
    Ping_Task     : Ping.Runner_Task;
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
    Ada.Text_IO.Put_Line("                                                            |_|   |_|              ");

    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    
    loop
      select
        accept Start
          (Self : Mapper_Task_Access)
        do
          Main_Task := Self;
        end Start;
        
        
        -- reading command line arguments
        Ada_Mr.Helper.Parse_Command_Line_Arguments(Ada_Mr.Helper.Mapper);
        
        
        -- print configuration
        Ada_Mr.Helper.Print_Configuration;
        
        
        -- start local server to accept incomming connections
        Server_Task.Start(
          GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "IP")),
          GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER", "PORT"))
        );
        
        
        -- runner task 
        Runner_Task.Start;
        
        -- ping task
        Ping_Task.Start;
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line("Depending tasks will be terminted", Ada_Mr.Logger.System);
        Ada_Mr.Mapper.Helper.Aborted.Stop;
        Runner_Task.Stop;
        Server_Task.Stop;
        abort Ping_Task;
        exit;
      or 
        accept Abort_It;
        Ada_Mr.Logger.Put_Line("Depending tasks will be aborted", Ada_Mr.Logger.System);
        Ada_Mr.Mapper.Helper.Aborted.Stop;
        abort Runner_Task;
        Server_Task.Stop;
        abort Ping_Task;
        exit;
      end select;
    end loop;
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      Ada_Mr.Mapper.Helper.Aborted.Stop;
      Runner_Task.Stop;
      Server_Task.Stop;
  end Mapper_Task;
  
  
  
  ----------------------
  -- Stop_Mapper_Task --
  ----------------------
  procedure Stop_Mapper_Task 
  is
  begin
    Main_Task.Stop;
  end Stop_Mapper_Task;
  
  
  
  -----------------------
  -- Abort_Mapper_Task --
  -----------------------
  procedure Abort_Mapper_Task
  is
  begin
    Main_Task.Abort_It;
  end Abort_Mapper_Task;
  
  
  
  -----------------
  -- Ping_Master --
  -----------------
  procedure Ping_Master
  is
    Master_Ip   : GNAT.Sockets.Inet_Addr_Type;
    Master_Port : GNAT.Sockets.Port_Type;
  begin
    Master_Ip   := GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("MASTER-IP"));
    Master_Port := GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("MASTER-PORT"));
    
    loop
      exit when Ada_Mr.Mapper.Helper.Aborted.Check = True;
      
      delay Duration(600);
      
      declare
        Response : String := Ada_Mr.Helper.Send(
          Master_Ip,
          Master_Port,
          Ada_Mr.Xml.Helper.Xml_Command(
            G_T          => Ada_Mr.Xml.Helper.Mapper,
            Command      => "ping",
            Access_Token => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN")
          )
        );
      begin
        null;
      end;
      
    end loop;
  end Ping_Master;
  
  
  
  ------------
  -- Banner --
  ------------
  function Banner
    return String 
  is
  begin
    return "ADA MR Mapper";
  end Banner;
  
end Ada_Mr.Mapper.Main;