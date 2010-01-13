with Ada.Text_IO;
with Ada.Exceptions;

with Ada_Mr.Xml.Parser;
with Ada_Mr.Xml.Helper;

with Ada_Mr.Logger;
with Ada_Mr.Crypt.Helper;


package body Ada_Mr.Generics.Echo is
  
  protected body Buffer is
    entry Deposit(X : in Echo_Access) when Count < Max_Tasks is
    begin
      Buf(I) := X;
      I := I + 1;
      Count := Count + 1;
    end Deposit;
    
    entry Extract(X : out Echo_Access) when Count > 0 is
    begin
      X := Buf(J);
      J := J + 1;
      Count := Count - 1;
    end Extract;
    
    function Num_Waiting return Natural is
    begin
      return Count;
    end Num_Waiting;
  end Buffer;
  
  
  
  task body Echo is 
    Sock : Socket_Type;
    S : Stream_Access;
    Input_Selector : Selector_Type;
    Input_Set : Socket_Set_Type;
    WSet : Socket_Set_Type;
    Input_Status : Selector_Status;
    Me : Echo_Access;
  begin 
    --set up selector 
    Create_Selector(Input_Selector);
    
    -- Initialise socket sets 
    -- WSet is always empty as we are not interested in output events 
    -- RSet only ever contains one socket namely Sock 
    Empty(Input_Set);
    Empty(WSet);
    
    accept Start(N_Sock : IN Socket_Type; Self : IN Echo_Access) do
      Sock := N_Sock;
      Me   := Self;
    end Start;
    
    loop
      -- block for exception handling
      begin
        
        -- set up stream on socket
        S := Stream(Sock);
        
        -- acknowledge connection
        Boolean'Write(S, True);
        
        loop
          -- check for input on Sock socket 
          Set(Input_Set, Sock);
          
          -- time-out on check if no input within 0.5 second 
          Check_Selector(Input_Selector, Input_Set, WSet, Input_Status, 0.5);
          
          -- we have input, so process it 
          if Input_Status = Completed then
            declare
              Request     : String := String'Input(S);
              Xml_Root    : Ada_Mr.Xml.Node_Access := Ada_Mr.Xml.Parser.Parse(Content => Request);
            begin
              
              Process_Request(S, Ada_Mr.Xml.Helper.Request_From(Xml_Root), Ada_Mr.Xml.Helper.Get_Verified_Content(Xml_Root));
            exception
              when Error : others =>
                Ada_Mr.Xml.Helper.Send_Error(S, Ada_Mr.Xml.Helper.Master, Error);
            end;
            
            exit; -- Shutdown socket after request is processed
          end if;
          
        end loop;
        
        Ada_Mr.Logger.Put_Line("Closing connection", Logger.Info);
        ShutDown_Socket(Sock, Shut_Read_Write);
        Buffer.Deposit(Me);
      exception
        when others =>
          Ada_Mr.Logger.Put_Line("Connection closed unexpectedly", Logger.Info);
          Close_Socket(Sock);
          Buffer.Deposit(Me);
      end;
      
      select
        accept ReStart (N_Sock : IN Socket_Type) DO
          Sock := N_Sock;
        end ReStart;
      or
        -- terminate if all slaves are queued here and 
        -- if the main server task has finished 
        terminate;
      end select;
    end loop;
  end Echo;
  
end Ada_Mr.Generics.Echo;