with Ada.Text_IO;
with Utility;
with Xml;
with Xml_Parser;
with Server;
with Worker;

package body Echo is
  
    task body Echo is 
      Sock : Socket_Type;
      S : Stream_Access;
      Me : Echo_Access;
      Input_Selector : Selector_Type;
      Input_Set : Socket_Set_Type;
      WSet : Socket_Set_Type;
      Input_Status : Selector_Status;
    begin 
      --set up selector 
      Create_Selector(Input_Selector);

      --Initialise socket sets 
      --WSet is always empty as we are not interested in output events 
      -- RSet only ever contains one socket namely Sock 
      Empty(Input_Set);
      Empty(WSet);

      ACCEPT Start(N_Sock : IN Socket_Type; Self : IN Echo_Access) DO
        Sock := N_Sock;
        Me := Self;
      end Start;

      loop
        -- block for exception handling
        begin
          -- set up stream on socket
          S := Stream(Sock);

          -- acknowledge connection
          Boolean'Write(S, True);


          --String'Output(S, "<xml version=""1.0"" />Initialier XML Kram ...");

          loop
            -- check for input on Sock socket 
            Set(Input_Set, Sock);

            -- time-out on check if no input within 0.5 second 
            Check_Selector(Input_Selector, Input_Set, WSet, Input_Status, 0.5);

--            Ada.Text_IO.Put(".");
            if Input_Status = Completed then
              -- we have input, so process it 
              
              declare
                Str : String := String'Input(S);
              begin
                Ada.Text_IO.Put_Line(Str);

                if Utility.Starts_With(Str, "<?xml") then
                  Ada.Text_IO.Put_Line("XML found!");
                  String'Output(S, "XML received: " & Str);

                  declare
                    Xml_Root : Xml.Node_Access := Xml_Parser.Parse(Content => Str);
                    Client_Type : String := Xml.Get_Value(Xml_Root, "client-type");
                  begin
                    if Utility.Is_Equal(Client_Type, "Mapper") or Utility.Is_Equal(Client_Type, "Reducer") then
                      Worker.Add_New_Worker(
                        Client_Type,
                        Xml.Get_Value(Xml_Root, "client-identifier"),
                        Me
                      );
                    else
                      Ada.Text_IO.Put_Line("Mist. Was anderes!");
                    end if;
                  end;

                else
                  Ada.Text_IO.Put_Line("Unknown command");
                  String'Output(S, "Unknown command: " & Str);
                end if;
  --              
              end;
            end if;

            if Server.Aborted.Check_Clients then
              String'Output(S, "Server aborted");
              exit;
            end if;

          end loop;

          Ada.Text_IO.New_Line;
          Ada.Text_IO.Put_Line("Slave Closing Connection");
          ShutDown_Socket(Sock, Shut_Read_Write);
  --        Buffer.Deposit(Me);

  --        exception 
            -- The mostly likely exception is if client quits unexpectedly 
            -- close the socket and deposit ourselves in the buffer 
  --          when others => 
  --               Ada.Text_IO.New_Line;
  --               Ada.Text_IO.Put_Line("Connection closed unexpectedly");
  --               Close_Socket(Sock);
  --               Buffer.Deposit(Me);
        end;

        select
          ACCEPT ReStart (N_Sock : IN Socket_Type) DO
            Sock := N_Sock;
          end ReStart;
        or
          -- terminate if all slaves are queued here and 
          -- if the main server task has finished 
          terminate;
        end select;
      end loop;
    end Echo;
  
end Echo;