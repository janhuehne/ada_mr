with Ada.Text_IO;

with Utility;
use Utility;

with Logger;
with Mapper_Helper;

with Xml_Helper;
with Ada.Exceptions;

package body Mapper is
  
----------------------------------------------------
-- MAPPER TASK                                    --
----------------------------------------------------
  task body Mapper_Task is
    R      : Runner.Runner_Task;
    S      : Server.Server.Server_Task;
  begin
    loop
      select
        accept Start;
        R.Start;
        S.Start(Mapper_Helper.Server_Bind_Ip, Mapper_Helper.Server_Bind_Port);
      or
        accept Stop;
          R.Stop;
          S.Stop;
        exit;
      end select;
    end loop;
  end Mapper_Task;
  
  
  
  
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
    
  end Parse_Configuration;
  
  
  procedure Process_User_Input(User_Input : String; To_Controll : Mapper_Task_Access) is
  begin
    if (Is_Equal(User_Input, "config", true)) then
      
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
    elsif (Is_Equal(User_Input, "start", true)) then
      To_Controll.Start;
    
    elsif (Is_Equal(User_Input, "quit", true)) or (Is_Equal(User_Input, "exit", true)) then
      Mapper_Helper.Aborted.Set_Exit;
      To_Controll.Stop;
    
    elsif (Is_Equal(User_Input, "abort", true)) then
      Mapper_Helper.Aborted.Set_Abort;
      To_Controll.Stop;
    
    elsif (Is_Equal(User_Input, "help", true)) then
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("  Commands:");
      Ada.Text_IO.Put_Line("    start        Starts the Ada MR Client");
      Ada.Text_IO.Put_Line("    abort        Stops the Ada MR Client immediately (current job is aborted)");
      Ada.Text_IO.Put_Line("    quit / exit  Stops the Ada MR Client after processing the last job");
    
    else
      Ada.Text_IO.Put_Line("Unknown command: " & User_Input);
    end if;
  end Process_User_Input;
end Mapper;