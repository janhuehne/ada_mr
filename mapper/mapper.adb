with Ada.Text_IO;

with Utility;
use Utility;

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
    Observer_Task : Observer.Observer_Task;
    
    procedure Read_and_Parse_Config_File(Config_File : String) is
    begin
      if Utility.Does_File_Exist(Config_File) then
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
        Utility.Print_Exception(Error);
        Ada.Exceptions.Raise_Exception(Utility.Configuration_File_Error'Identity, "There is a problem with the configuration file.");
    end Read_and_Parse_Config_File;
    
    procedure Print_Configuration is
    begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("-> Ada MR Mapper configuration");
      
      Utility.Put("Identifier:", 20, 2);
      Utility.Put(ASU.To_String(Mapper_Helper.Identifier), 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Access token:", 20, 2);
      Utility.Put(Mapper_Helper.Access_Token, 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Listen on ip:", 20, 2);
      Utility.Put(GNAT.Sockets.Image(Mapper_Helper.Server_Bind_Ip), 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Listen on port:", 20, 2);
      Utility.Put(Mapper_Helper.Server_Bind_Port'Img, 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Master host:", 20, 2);
      Utility.Put(GNAT.Sockets.Image(Mapper_Helper.Master_Ip), 60, 2);
      Ada.Text_IO.New_Line;
      
      Utility.Put("Master port:", 20, 2);
      Utility.Put(Mapper_Helper.Master_Port'Img, 60, 2);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Print_Configuration;
    
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
          Read_and_Parse_Config_File(Config_File);
        end Start;
        
        -- print configuration
        Print_Configuration;
        
        -- start local server to accept incomming connections
        Server_Task.Start(Mapper_Helper.Server_Bind_Ip, Mapper_Helper.Server_Bind_Port);
        
        -- observer task
        Observer_Task.Start(Main_Task);
        
        -- runner task 
        Runner_Task.Start;
      or
        accept Stop;
        Logger.Put_Line("Depending tasks will be terminted", Logger.Info);
        Runner_Task.Stop;
        Server_Task.Stop;
        Observer_Task.Stop;
        exit;
      end select;
    end loop;
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
  
  
  procedure Parse_Configuration(Config_Xml : Xml.Node_Access) is
  begin
    Mapper_Helper.Identifier            := ASU.To_Unbounded_String(Xml.Get_Value(Config_Xml, "identifier"));
    
    declare
      Local_Server_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "local_server");
    begin
      Mapper_Helper.Server_Bind_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Local_Server_Details, "bind_ip"));
      Mapper_Helper.Server_Bind_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Local_Server_Details, "bind_port"));
    end;
    
    declare
      Master_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "master");
    begin
      Mapper_Helper.Master_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Master_Details, "ip"));
      Mapper_Helper.Master_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Master_Details, "port"));
    end;
    
    declare
      Reducer_Details : Xml.Node_Access := Xml.Find_Child_With_Tag(Config_Xml, "reducer");
    begin
      Mapper_Helper.Reducer_Ip   := GNAT.Sockets.Inet_Addr(Xml.Get_Value(Reducer_Details, "ip"));
      Mapper_Helper.Reducer_Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Reducer_Details, "port"));
    end;
    
  end Parse_Configuration;
  
  
----------------------------------------------------
-- GENERIC OBSERVER TASK                          --
----------------------------------------------------
  function Exit_Observer return Boolean is
  begin
    if Mapper_Helper.Aborted.Get_Exit = true OR Mapper_Helper.Aborted.Get_Abort = true then
      return true;
    end if;
      
    return false;
  end Exit_Observer;
  
  
  function Observe(To_Controll : Mapper_Task_Access) return Boolean is
  begin
    
    if Mapper_Helper.Aborted.Get_Exit = true OR Mapper_Helper.Aborted.Get_Abort = true then
      To_Controll.Stop;
      return true;
    end if;
    
    return false;
  end Observe;
end Mapper;