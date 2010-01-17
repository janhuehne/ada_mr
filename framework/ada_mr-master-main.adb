with Ada.Text_IO;
with Ada.Calendar;
with GNAT.Calendar.Time_IO;

with Ada_Mr.Helper;
use Ada_Mr.Helper;

--with Server;
with Ada_Mr.Logger;

--with Worker;
--with Xml_Queue;
with Ada_Mr.Xml.Helper;
with GNAT.Sockets;

with Ada.Exceptions;
with Ada_Mr.Crypt.Helper;
with Ada_Mr.Xml.Parser;

package body Ada_Mr.Master.Main is
  
  task body Master_Task is
    Server_Task    : Server.Server.Server_Task;
    Observer_Task  : Observer.Runner_Task;
    Console_Task   : Console.Console;
  begin
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;

    Ada.Text_IO.Put_Line("          _____               __  __ _____      __  __           _            ");
    Ada.Text_IO.Put_Line("    /\   |  __ \   /\        |  \/  |  __ \    |  \/  |         | |           ");
    Ada.Text_IO.Put_Line("   /  \  | |  | | /  \ ______| \  / | |__) |   | \  / | __ _ ___| |_ ___ _ __ ");
    Ada.Text_IO.Put_Line("  / /\ \ | |  | |/ /\ \______| |\/| |  _  /    | |\/| |/ _` / __| __/ _ \ '__|");
    Ada.Text_IO.Put_Line(" / ____ \| |__| / ____ \     | |  | | | \ \    | |  | | (_| \__ \ |_  __/ |   ");
    Ada.Text_IO.Put_Line("/_/    \_\_____/_/    \_\    |_|  |_|_|  \_\   |_|  |_|\__,_|___/\__\___|_|   ");

    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    Ada.Text_IO.New_Line;
    
    loop
      select
        accept Start(Self : Master_Task_Access) do
          Main_Task := Self;
        end Start;
        
        
        -- set default configuration
        Ada_Mr.Helper.Set_Default_Configuration(Ada_Mr.Helper.Master);
        
        
        -- reading command line arguments
        Ada_Mr.Helper.Parse_Command_Line_Arguments(Ada_Mr.Helper.Master);
        
        
        -- print configuration
        Ada_Mr.Helper.Print_Configuration;
        
        Ada_Mr.Logger.Put_Line("Splitting raw data", Ada_Mr.Logger.Info);
        Split_Raw_Data;
        Ada_Mr.Logger.Put_Line("Splitting raw data done", Ada_Mr.Logger.Info);
        
        Ada_Mr.Logger.Put_Line("Importing jobs", Ada_Mr.Logger.Info);
        
        loop
          declare
          begin
            Jobs.Add(Get_Next_Raw_Job);
          exception
            when Constraint_Error => exit;
          end;
        end loop;
        
        Ada_Mr.Logger.Put_Line(Jobs.Count'Img & " jobs imported", Ada_Mr.Logger.Info);
        
        Console_Task.Start(
          Main_Task
        );
        
        -- start local server to accept incomming connections
        Server_Task.Start(
          GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER-BIND_IP")),
          GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("LOCAL_SERVER-BIND_PORT"))
        );
        
        Observer_Task.Start;
      or
        accept Stop;
        Ada_Mr.Logger.Put_Line(" -> Please wait, while closing the client connections.", Ada_Mr.Logger.Info);
        
        -- sending exit command to all worker
        declare
          
          procedure Send_Exit_Command(C : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor) is
            Worker_Access : Ada_Mr.Master.Helper.Worker_Record_Access;
          begin
            Worker_Access :=  Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(C);
            
            declare
              Xml_Command : String := Ada_Mr.Xml.Helper.Xml_Command(
                G_T           => Ada_Mr.Xml.Helper.Master,
                Command       => "exit",
                Access_Token  => Worker_Access.Access_Token
              );
              
              Response : String := Ada_Mr.Helper.Send(
                Worker_Access.Ip,
                Worker_Access.Port,
                Xml_Command
              );
            begin
              Logger.Put_Line(Response, Logger.Info);
            end;
          exception
            when others =>
              Logger.Put_Line("Worker """ & ASU.To_String(Worker_Access.Identifier) & """ could not shut down.", Logger.Err);
          end;
          
          Worker_Vector : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector;
        begin
          Worker_Vector.Append(Worker.Find_All_By_Type(Ada_Mr.Helper.Mapper));
          Worker_Vector.Append(Worker.Find_All_By_Type(Ada_Mr.Helper.Reducer));
          
          Worker_Vector.Iterate(Send_Exit_Command'Access);
        end;
        
        
        Ada_Mr.Master.Helper.Aborted.Set_Exit;
        Server_Task.Stop;
        Observer_Task.Stop;
        abort Console_Task;
        exit;
      end select;
    end loop;
  exception
    when Error : others =>
      Ada_Mr.Helper.Print_Exception(Error);
      Ada_Mr.Master.Helper.Aborted.Set_Exit;
      Server_Task.Stop;
      Observer_Task.Stop;
      abort Console_Task;
  end Master_Task;
  
  procedure Stop_Master_Task is
  begin
    Main_Task.Stop;
  end Stop_Master_Task;
  
  
  
  ----------------------------------------------------
  -- GENERIC OBSERVER TASK                           -
  ----------------------------------------------------
  function Exit_Observer return Boolean is
  begin
    if Ada_Mr.Master.Helper.Aborted.Get_Exit = true OR Ada_Mr.Master.Helper.Aborted.Get_Abort = true then
      return true;
    end if;
      
    return false;
  end Exit_Observer;
  
  
  --function Observe(To_Controll : Master_Task_Access) return Boolean is
  procedure Observe is
    use GNAT.Sockets;
  begin
    loop
      exit when Exit_Observer;
      
      if Jobs.Count_By_State(Ada_Mr.Master.Helper.Done) = Jobs.Count then
      
        Ada_Mr.Logger.Put_Line("All jobs done", Ada_Mr.Logger.Info);
        
        -- TODO: send this to all connected reducers!
        declare
          Reducer_Vector : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector := Worker.Find_All_By_Type(Ada_Mr.Helper.Reducer);
          
          procedure Send_Finalize(C : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor) is
            Reducer : Ada_Mr.Master.Helper.Worker_Record_Access := Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(C);
          begin
            declare
              Response : String := Ada_Mr.Helper.Send(
                Reducer.Ip,
                Reducer.Port,
                Ada_Mr.Xml.Helper.Xml_Command(Ada_Mr.Xml.Helper.Master, "finalize"),
                5
              );
            begin
              null;
            end;
          end Send_Finalize;
        begin
          Reducer_Vector.Iterate(Send_Finalize'Access);
          exit;
        end;
        
    --    return true;
      end if;
    end loop;
    
    Main_Task.Stop;
  --  return false;
  end Observe;



  function "="(Left, Right : Job_Entry_Record_Access) return Boolean is
  begin
    return true;
  end "=";
  
  
  
  protected body Jobs is
  
    procedure Add(Job : My_Job) is
      Job_Entry : Job_Entry_Record_Access := new Job_Entry_Record;
    begin
      Job_Entry.Job := Job;
      Job_Entry.State := Ada_Mr.Master.Helper.Pending;
        
      Jobs.Append(Job_Entry);
      Ada_Mr.Logger.Put_Line("--> Job successfully imported.", Ada_Mr.Logger.Info);
    end Add;
    
    function Get_By_Id(Id : Natural) return Job_Entry_Record_Access is
      Cursor : Job_Entry_Record_Vectors.Cursor := Jobs.First;
    begin
      loop
        exit when Job_Entry_Record_Vectors."="(Cursor, Job_Entry_Record_Vectors.No_Element);
        
        declare 
          Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
        begin
          if Get_Job_Id(Element.Job) = Id then
            return Element;
          end if;
        end;
        
        Job_Entry_Record_Vectors.Next(Cursor);
      end loop;
      
      return null;
    end Get_By_Id;
    
    function Get_Next_Pending return Job_Entry_Record_Access is
      Cursor : Job_Entry_Record_Vectors.Cursor := Jobs.First;
    begin
      loop
        exit when Job_Entry_Record_Vectors."="(Cursor, Job_Entry_Record_Vectors.No_Element);
        
        declare 
          Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
        begin
          if Ada_Mr.Master.Helper."="(Element.State, Ada_Mr.Master.Helper.Pending) then
            Element.State := Ada_Mr.Master.Helper.In_Progress;
            return Element;
          end if;
        end;
        
        Job_Entry_Record_Vectors.Next(Cursor);
      end loop;
      
      Ada.Exceptions.Raise_Exception(Ada_Mr.Master.Helper.No_Job_Found'Identity, "No futher job found.");
    end Get_Next_Pending;
    
    function Count return Natural is
    begin
      return Natural(Jobs.Length);
    end Count;
    
    function Count_By_State(State : Ada_Mr.Master.Helper.Job_State) return Natural is
      use Ada_Mr.Master.Helper;
      
      Counter : Natural := 0;
      
      procedure Count(Cursor : Job_Entry_Record_Vectors.Cursor) is
        Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
      begin
        if Element.State = State then
          Counter := Counter + 1;
        end if;
      end Count;
    
    begin
      Jobs.Iterate(Count'Access);
      
      return Counter;
    end Count_By_State;
    
    procedure Print is
      
      procedure Print(Cursor : Job_Entry_Record_Vectors.Cursor) is
        Element : Job_Entry_Record_Access := Job_Entry_Record_Vectors.Element(Cursor);
      begin
        Print_Job(Element.Job, Ada_Mr.Master.Helper.To_String(Element.State));
      end Print;
      
    begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Jobs:");
      Ada.Text_IO.New_Line;
      
      Jobs.Iterate(Print'Access);
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Print;
  end Jobs;
  
  
  procedure Change_Job_State(Job_Entry : in out Job_Entry_Record_Access; State : Ada_Mr.Master.Helper.Job_State; Message : String := "") is
  begin
    Job_Entry.State := State;
    
    if Message /= "" then
      Job_Entry.Message := ASU.To_Unbounded_String(Message);
    end if;
    
  end Change_Job_State;
  
  
  function Job_Entry_To_Xml(Job_Entry : Job_Entry_Record_Access) return String is
  begin
    return To_Xml(Job_Entry.Job);
  end Job_Entry_To_Xml;
  
  function Job_Is_Null(Job_Entry : Job_Entry_Record_Access) return Boolean is
  begin
    return Job_Entry = Null;
  end Job_Is_Null;
  
  
  protected body Worker is
  
    procedure Add(New_Worker : Ada_Mr.Master.Helper.Worker_Record_Access) is
      Cursor : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor := Worker.First;
    begin
      New_Worker.Access_Token := Ada_Mr.Crypt.Helper.Create_Access_Token(
        ASU.To_String(New_Worker.Identifier),
        Ada_Mr.Helper.To_String(New_Worker.W_Type)
      );
      
      New_Worker.Updated_At := Ada.Calendar.Clock;
      
      loop
        exit when Ada_Mr.Master.Helper.Worker_Entry_Vectors."="(Cursor, Ada_Mr.Master.Helper.Worker_Entry_Vectors.No_Element);
        
        declare
          Old_Worker : Ada_Mr.Master.Helper.Worker_Record_Access := Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(Cursor);
        begin
          
          if Ada_Mr.Helper.Is_Equal(ASU.To_String(Old_Worker.Identifier), New_Worker.Identifier) then
            Worker.Delete(Cursor);
            exit;
          end if;
        end;
        
        Ada_Mr.Master.Helper.Worker_Entry_Vectors.Next(Cursor);
      end loop;
      
      
      Worker.Append(New_Worker);
    end Add;
    
    function Find_By_Identifier(Identifier : String) return Ada_Mr.Master.Helper.Worker_Record_Access is
      Cursor : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor := Worker.First;
    begin
      loop
        exit when Ada_Mr.Master.Helper.Worker_Entry_Vectors."="(Cursor, Ada_Mr.Master.Helper.Worker_Entry_Vectors.No_Element);
        
        declare
          Worker : Ada_Mr.Master.Helper.Worker_Record_Access := Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(Cursor);
        begin
          
          if Ada_Mr.Helper.Is_Equal(ASU.To_String(Worker.Identifier), Identifier) then
            return Worker;
          end if;
        end;
        
        Ada_Mr.Master.Helper.Worker_Entry_Vectors.Next(Cursor);
        
      end loop;
      
      Ada.Exceptions.Raise_Exception(Ada_Mr.Master.Helper.No_Worker_Found'Identity, "No worker found");
    end Find_By_Identifier;
    
    
    function Find_By_Access_Token_And_Type(Access_Token : String; W_Type : Ada_Mr.Helper.Worker_Type) return Ada_Mr.Master.Helper.Worker_Record_Access is
      Cursor : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor := Worker.First;
    begin
      loop
        exit when Ada_Mr.Master.Helper.Worker_Entry_Vectors."="(Cursor, Ada_Mr.Master.Helper.Worker_Entry_Vectors.No_Element);
        
        declare
          Worker : Ada_Mr.Master.Helper.Worker_Record_Access := Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(Cursor);
        begin
          
          if Ada_Mr.Helper."="(Worker.W_Type, W_Type) and Worker.Access_Token = Access_Token then
            return Worker;
          end if;
        end;
        
        Ada_Mr.Master.Helper.Worker_Entry_Vectors.Next(Cursor);
        
      end loop;
      
      Ada.Exceptions.Raise_Exception(Ada_Mr.Master.Helper.No_Worker_Found'Identity, "No worker found");
      
    end Find_By_Access_Token_And_Type;
    
    
    function Find_All_By_Type(W_Type : Ada_Mr.Helper.Worker_Type) return Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector is
      Type_Vector : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector;
      
      procedure Find(C : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor) is
        Worker : Ada_Mr.Master.Helper.Worker_Record_Access := Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(C);
      begin
        if Ada_Mr.Helper."="(Worker.W_Type, W_Type) then
          Type_Vector.Append(Worker);
        end if;
      end Find;
    begin
      Worker.Iterate(Find'Access);
      
      return Type_Vector;
    end Find_All_By_Type;
    
    procedure Print is
      
      procedure Print(Cursor : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Cursor) is
        Worker_Entry : Ada_Mr.Master.Helper.Worker_Record_Access := Ada_Mr.Master.Helper.Worker_Entry_Vectors.Element(Cursor);
      begin
        Ada_Mr.Helper.Put(ASU.To_String(Worker_Entry.Identifier), 30, 2);
        Ada_Mr.Helper.Put(Ada_Mr.Helper.To_String(Worker_Entry.W_Type), 10, 2);
        Ada_Mr.Helper.Put(Worker_Entry.Access_Token, 40, 2);
        Ada_Mr.Helper.Put(GNAT.Sockets.Image(Worker_Entry.Ip), 20, 2);
        Ada_Mr.Helper.Put(Worker_Entry.Port'Img, 20, 2);
        Ada_Mr.Helper.Put(GNAT.Calendar.Time_IO.Image(Worker_Entry.Updated_At, "%Y-%m-%d %H:%M:%S"), 30, 2);
        Ada.Text_IO.New_Line;
      end Print;
      
    begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("Connected worker:");
      Ada.Text_IO.New_Line;
      Ada_Mr.Helper.Put("Identifier", 30, 2);
      Ada_Mr.Helper.Put("Type", 10, 2);
      Ada_Mr.Helper.Put("Access Token", 40, 2);
      Ada_Mr.Helper.Put("IP address", 20, 2);
      Ada_Mr.Helper.Put("Listen on port", 20, 2);
      Ada_Mr.Helper.Put("Updated at", 30, 2);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line("------------------------------------------------------------------------------------------------------------------");
      
      Worker.Iterate(Print'Access);
      
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Print;
  
  end Worker;
  
  
  
  ----------------------------------------------------
  -- GENERIC CONSOLE METHODS                        --
  ----------------------------------------------------
  function Banner return String is
  begin
    return "ADA MR Master";
  end Banner;
  
  
  procedure Process_User_Input(User_Input : String; To_Controll : Master_Task_Access) is
  begin
    if (Is_Equal(User_Input, "help", true)) then
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("  Commands:");
      Ada.Text_IO.Put_Line("    worker       Prints all connected worker");
      Ada.Text_IO.Put_Line("    config       Prints configuration");
      Ada.Text_IO.Put_Line("    quit         Exit Ada MR Master and stop all mapper and reducer");
      Ada.Text_IO.Put_Line("    jobs         Number of unprocessed jobs");
      Ada.Text_IO.Put_Line("    help         Displays this message");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    
    elsif (Ada_Mr.Helper.Is_Equal(User_Input, "config", true)) then
      Ada_Mr.Helper.Print_Configuration;
    
    elsif Ada_Mr.Helper.Is_Equal(User_Input, "quit", true) OR Is_Equal(User_Input, "exit", true) then
      To_Controll.Stop;
    
    elsif (Is_Equal(User_Input, "worker", true)) then
      Worker.Print;
    
    elsif (Is_Equal(User_Input, "jobs", true)) then
      Jobs.Print;
    
    else
      Ada.Text_IO.Put_Line("Unknown command: " & User_Input);
    end if;
  end Process_User_Input;
  
  
  
  
  
  
end Ada_Mr.Master.Main;