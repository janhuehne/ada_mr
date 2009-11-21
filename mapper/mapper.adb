with Ada.Text_IO;

with Utility;
use Utility;

with Logger;
with Mapper_Helper;

with Xml_Helper;
with Ada.Exceptions;

package body Mapper is
  
  task body Mapper_Task is
    R      : Runner.Runner_Task;
    S      : Server.Server_Task;

  begin
    loop
      select
        accept Start;
        
        R.Start;
        S.Start;
      or
        accept Stop;
          R.Stop;
          S.Stop;
        exit;
      end select;
    end loop;
  end Mapper_Task;
  
  
  task body Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    C               : Mapper_Task_Access;
    Config          : Xml.Node_Access;
  begin
    accept Start(C_Arg : Mapper_Task_Access; Config_Xml : Xml.Node_Access) do
      C      := C_Arg;
      Config := Config_Xml;
    end Start;
    
    Ada.Text_IO.Put_Line("Client Console");
    
    Mapper_Helper.Identifier     := ASU.To_Unbounded_String(Xml.Get_Value(Config, "identifier"));
    
    declare
    begin
      Mapper_Helper.Listen_Sock_Addr.Addr := GNAT.Sockets.Addresses(GNAT.Sockets.Get_Host_By_Name("127.0.0.1"), 1);
      Mapper_Helper.Listen_Sock_Addr.Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Config, "listen_on_port"));
      
      Mapper_Helper.Master_Sock_Addr.Addr := GNAT.Sockets.Addresses(GNAT.Sockets.Get_Host_By_Name(Xml.Get_Value(Config, "master_host")), 1);
      Mapper_Helper.Master_Sock_Addr.Port := GNAT.Sockets.Port_Type'Value(Xml.Get_Value(Config, "master_port"));
    exception
      when Error : others => 
        Utility.Print_Exception(Error);
        Ada.Exceptions.Raise_Exception(Utility.Configuration_File_Error'Identity, "There is a problem with the configuration file.");
    end;
    
    Ada.Text_IO.Put(":> ");
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        
        if (Is_Equal(In_String, In_Last, "config", true)) then
          
          Ada.Text_IO.New_Line;
          Ada.Text_IO.Put_Line("-> Ada MR Mapper configuration");
          
          Utility.Put("Identifier:", 20, 2);
          Utility.Put(ASU.To_String(Mapper_Helper.Identifier), 60, 2);
          Ada.Text_IO.New_Line;
          
          Utility.Put("Access token:", 20, 2);
          Utility.Put(Mapper_Helper.Access_Token, 60, 2);
          Ada.Text_IO.New_Line;
          
          Utility.Put("Listen on port:", 20, 2);
          Utility.Put(Mapper_Helper.Listen_Sock_Addr.Port'Img, 60, 2);
          Ada.Text_IO.New_Line;
          
          Utility.Put("Master host:", 20, 2);
          Utility.Put(GNAT.Sockets.Image(Mapper_Helper.Master_Sock_Addr.Addr), 60, 2);
          Ada.Text_IO.New_Line;
          
          Utility.Put("Master port:", 20, 2);
          Utility.Put(Mapper_Helper.Master_Sock_Addr.Port'Img, 60, 2);
          Ada.Text_IO.New_Line;
          Ada.Text_IO.New_Line;
          Ada.Text_IO.New_Line;
        elsif (Is_Equal(In_String, In_Last, "start", true)) then
          C.Start;
        
        elsif (Is_Equal(In_String, In_Last, "quit", true)) or (Is_Equal(In_String, In_Last, "exit", true)) then
          Mapper_Helper.Aborted.Set_Exit;
          C.Stop;
          exit;
        
        elsif (Is_Equal(In_String, In_Last, "abort", true)) then
          Mapper_Helper.Aborted.Set_Abort;
          C.Stop;
          exit; 
        
        elsif (Is_Equal(In_String, In_Last, "help", true)) then
          Ada.Text_IO.Put_Line("");
          Ada.Text_IO.Put_Line("  Commands:");
          Ada.Text_IO.Put_Line("    start        Starts the Ada MR Client");
          Ada.Text_IO.Put_Line("    abort        Stops the Ada MR Client immediately (current job is aborted)");
          Ada.Text_IO.Put_Line("    quit / exit  Stops the Ada MR Client after processing the last job");
        
        else
          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
        end if;
      end if;
--        
      Ada.Text_IO.Put(":> ");
    end loop;
  exception
    when Error : Utility.Configuration_File_Error =>
      C.Stop;
      Utility.Print_Exception(Error);
  end Console;
    
end Mapper;