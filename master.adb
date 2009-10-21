with Ada.Text_IO;


with Utility;
use Utility;

with Server;
with Logger;

with Worker;
with Xml_Queue;

package body Master is
  
  task body Master_Task is
    Main_Server : Server.P_Server;
    Me          : Master_Task_Access;
  begin
    loop
      select
        accept Import_Jobs(Jobs : Job_Vector.Vector) do
          Unprocessed_Jobs := Jobs;
        end Import_Jobs;
        
        Ada.Text_IO.Put_Line(Unprocessed_Jobs.Length'Img);
      or
        accept Start_Master(M : Master_Task_Access) do
          Me := M;
        end Start_Master;
        
        Ada.Text_IO.Put_Line("Starting Master Service");
        
        declare
          Cursor : Job_Vector.Cursor := Unprocessed_Jobs.First;
        begin
          Ada.Text_IO.Put_Line("  .. Preparing job containers");
          loop
            Xml_Queue.Add_Job(Get_Job_Id(Job_Vector.Element(Cursor)), To_Xml(Job_Vector.Element(Cursor)));
            Job_Vector.Next(Cursor);
            
            exit when Job_Vector."="(Cursor, Job_Vector.No_Element);
          end loop;
          Ada.Text_IO.Put_Line("      -> " & Xml_Queue.Count_Jobs(Xml_Queue.Pending)'Img & " jobs imported.");
        end;
        
        Main_Server.Start;
      or
        accept Split_Data;
        Ada.Text_IO.Put_Line("Split Data!"); 
      or
        accept Add_Job;
        Ada.Text_IO.Put_Line("Calling add job!"); 
      or
        accept Stop_Master;
        Ada.Text_IO.Put_Line("Please wait, while closing the client connections.");
        Server.Aborted.Stop_Master;
        Main_Server.Stop;
        exit;
      or
        accept Say_Hello;
        Ada.Text_IO.Put_Line("Hello!");
      end select;
    end loop;
  end Master_Task;
  
--   task body Job_Management_Task is
--   begin
--     Ada.Text_IO.Put_Line("Job Management started.");
--     
--     loop
--       exit when Server.Aborted.Check_Master = true;
--       
--       if not Unprocessed_Jobs.Is_Empty and not Worker.Idle_Mapper.Is_Empty then
--         declare
--           Next_Job : My_Job               := Get_Next_Job;
--           Mapper   : Worker.Worker_Access := Worker.Get_Idle_Mapper;
--         begin
--           
-- --          String'Output(Mapper.W_Echo.S, "Jetzt wird mal ein Job ausgeliefert!");
--           
--           Jobs_In_Progress.Append(Next_Job);
--           Worker.Active_Mapper.Append(Mapper);
--         end;
--         
--         Ada.Text_IO.Put_Line("Wir haben was zu tun! Jobs und Mapper sind da.");
--       end if;
--       
-- --      Ada.Text_IO.Put_Line(
-- --        Unprocessed_Jobs.Length'Img & " -- " & Worker.Idle_Mapper.Length'Img
-- --      );
--       
--       
--     end loop;    
--   end Job_Management_Task;
  
  
  task body Master_Console is
    In_String       : String(1..20);
    In_Last         : Natural;
    M               : Master_Task_Access;
  begin
    accept Start(M_Arg : Master_Task_Access) do
      M := M_Arg;
    end Start;
    
    Ada.Text_IO.Put_Line("Master Console");
    
    Ada.Text_IO.Put(":> ");
    
    loop
      Ada.Text_IO.Get_Line(In_String, In_Last);
      
      if In_Last > 0 then
        if (Is_Equal(In_String, In_Last, "start", true)) then
          M.Start_Master(M);
          
        elsif (Is_Equal(In_String, In_Last, "quit", true)) then
          M.Stop_Master;
          exit; 
          
        elsif (Is_Equal(In_String, In_Last, "help", true)) then
          Ada.Text_IO.Put_Line("");
          Ada.Text_IO.Put_Line("  Commands:");
          Ada.Text_IO.Put_Line("    start        Starts the Ada MR Master Server");
          Ada.Text_IO.Put_Line("    idle-mappers Prints the idle workers");
          Ada.Text_IO.Put_Line("    quit         Exit Ada MR Server Server");
          Ada.Text_IO.Put_Line("    verbose-on   Enable verbose mode to display log entries");
          Ada.Text_IO.Put_Line("    verbose-off  Disable verbose mode");
          Ada.Text_IO.Put_Line("    jobs         Number of unprocessed jobs");
          Ada.Text_IO.Put_Line("    help         Displays this message");
          Ada.Text_IO.New_Line;
          Ada.Text_IO.New_Line;
          Ada.Text_IO.New_Line;
          
        elsif (Is_Equal(In_String, In_Last, "verbose-on", true)) then
          Logger.Enable_Verbose_Mode;
          Ada.Text_IO.Put_Line("Verbose mode: On");
        
        elsif (Is_Equal(In_String, In_Last, "verbose-off", true)) then
          Logger.Disable_Verbose_Mode;
          Ada.Text_IO.Put_Line("Verbose mode: Off");
        elsif (Is_Equal(In_String, In_Last, "idle-mappers", true)) then
          Worker.Print_All_Idle_Mapper;
        elsif (Is_Equal(In_String, In_Last, "jobs", true)) then
--          Ada.Text_IO.Put_Line(Unprocessed_Jobs.Length'Img & " unprocessed jobs");
          Xml_Queue.Print_Jobs;
        else
          Ada.Text_IO.Put_Line("Unknown command: " & In_String(1..In_Last));
        end if;
      end if;
        
      Ada.Text_IO.Put(":> ");
    end loop;
    
  end Master_Console;
  
  procedure Add_New_Job(Job : My_Job) is
  begin
    Unprocessed_Jobs.Append(Job);
  end Add_New_Job;
  
  function Get_Next_Job(Remove_From_Vector : Boolean := true) return My_Job is
    Job_Cursor : Job_Vector.Cursor := Unprocessed_Jobs.First;
    Job        : My_Job            := Unprocessed_Jobs.Element(Job_Vector.To_Index(Job_Cursor));
  begin
    if Remove_From_Vector = true then
      Unprocessed_Jobs.Delete(Job_Vector.To_Index(Job_Cursor));
    end if;
    
    return Job;
  end Get_Next_Job;
  
end Master;