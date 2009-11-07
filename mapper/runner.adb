with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Utility;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Xml;
with Xml_Parser;
with Xml_Helper;

with Ada.Exceptions;

package body Runner is 
  
  protected body Aborted is
  
    procedure Stop is
    begin
      Abort_It := true;
    end Stop;
    
    function Check return Boolean is
    begin
      return Abort_It;
    end Check;
    
  end Aborted;
  
  task body Runner_Task is
    Sock            : Socket_Type;
    S               : Stream_Access;
    Addr            : Sock_Addr_Type (Family_Inet);
    Msg             : String (1 .. 2000);
    Last            : Natural;
    B               : Boolean;
    Read_Selector   : Selector_Type;
    Read_Set, WSet  : Socket_Set_Type;
    Read_Status     : Selector_Status;
  begin
    loop
      select
        accept Start;
        
        Ada.Text_IO.Put_Line("Runner Task started!");
        
        Initialize;
        Create_Socket(Sock);
        Addr.Addr := Addresses(Get_Host_By_Name ("127.0.0.1"), 1);
        Addr.Port := 7000;
        
        Create_Selector(Read_Selector);
        Empty(Read_Set);
        Empty(WSet);
        
        Connect_Socket(Sock, Addr);
        S := Stream (Sock);
        Boolean'Read (S, B);
        
        
        Set(Read_Set, Sock);
        
        -- check for input on socket (server may be aborting)
        -- time-out immediately if no input pending
        -- We seem to need a small delay here (using zero seems to block
        -- forever)
        -- Is this a GNAT bug or AB misreading Check_Selector docs?
        Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
        
        String'Output(
          S, 
          Xml_Helper.Create_Initialization(Xml_Helper.Mapper, "Mapper_01")
        );
        
        declare
          Str : String := String'Input(S);
        begin
          Ada.Text_IO.Put_Line(Str);
        end;
        
        
        loop 
          exit when Aborted.Check = true;
          
          Set(Read_Set, Sock);
          
          -- check for input on socket (server may be aborting)
          -- time-out immediately if no input pending
          -- We seem to need a small delay here (using zero seems to block
          -- forever)
          -- Is this a GNAT bug or AB misreading Check_Selector docs?
          Check_Selector(Read_Selector, Read_Set, WSet, Read_Status, 0.005);
          
          if Read_Status = Expired then
            
            -- ask for new job
            String'Output(
              S, 
              Xml_Helper.Create_Job_Request
            );
            
--            Ada.Text_IO.Put_Line("Server Request: " & Xml_Helper.Create_Job_Request);
          
            declare
              -- receive message
              Str : String := String'Input(S);
            begin
--              Ada.Text_IO.Put_Line("Server Response: " & Str);
              
              if Xml_Helper.Is_Valid_Xml_String(Str) then
                
                declare
                  Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
                begin
                  if Utility.Is_Equal(Xml.Get_Tag(Xml_Root), "adamr-master") then
                    if Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "new_job") then
                      declare
                        Job : My_Job := From_Xml(Xml.Find_Child_With_Tag(Xml_Root, "details"));
                      begin
                        if Compute_Job(Job) then
                                                  
                          -- send later to the responding reducer!
                          String'Output(
                            S, 
                            Xml_Helper.Xml_Command(Xml_Helper.Mapper, "job_done", To_Xml(Job))
                          );
                          
                          
                          -- TODO: Error handling, if happens!
                          declare
                            Str : String := String'Input(S);
                          begin
                            null;
                          end;
--                          Ada.Text_IO.Put_Line("Server Request: " & Xml_Helper.Xml_Command(Xml_Helper.Mapper, "job_done", To_Xml(Job)));
--                          Ada.Text_IO.Put_Line("Server Response: " & String'Input(S));
                          
                          Ada.Text_IO.Put_Line(Job_Result_To_Xml);
                        else
                          Ada.Text_IO.Put_Line("Job Fehler!!!!");
                        end if;
                      end;
                    elsif Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "exit") then
                      exit;
                    elsif Utility.Is_Equal(Xml.Get_Value(Xml_Root, "command"), "sleep") then
                      declare
                        Time : Float := Float'Value(Xml.Get_Value(Xml.Find_Child_With_Tag(Xml_Root, "details"), "seconds"));
                      begin
                        Ada.Text_IO.Put_Line("No further job found. Waiting " & Float'Image(Time) & " seconds until next job request!");
                        delay Duration(Time);
                      end;
                    else
                      Ada.Text_IO.Put_Line("Unknown command!");
                    end if;
                  end if;
                end;
              else
                Ada.Text_IO.Put_Line("Not a valid xml response!");
              end if;
              
              exit when Str = "Server aborted";
            end;
            
          end if;
          
          Ada.Text_IO.New_Line;
          
        end loop;
        
        --  tidy up 
        ShutDown_Socket(Sock);
        Close_Selector(Read_Selector);
        Finalize;
        
        Ada.Text_IO.New_Line;
        Ada.Text_IO.Put_Line("Mapper task exiting ...");
        Finalize;
        exit;
      end select;
    end loop;
  exception 
    when Error : others =>
      Ada.Text_IO.Put_Line ("Exception: Client quitting ..." ) ;
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(Error));
      
      Close_Socket(Sock);
      Close_Selector(Read_Selector);
      Finalize;
  end Runner_Task;
    
end Runner;