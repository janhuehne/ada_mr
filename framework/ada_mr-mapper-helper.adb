with Ada_Mr.Logger;
with Ada_Mr.Xml;
with Ada_Mr.Xml.Helper;
with Ada_Mr.Xml.Parser;

package body Ada_Mr.Mapper.Helper is
  
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
  
  procedure Send_Result(Reducer_Result_Map : Ada_Mr.Helper.String_String_Maps.Map) is
    Master_Ip   : GNAT.Sockets.Inet_Addr_Type;
    Master_Port : GNAT.Sockets.Port_Type;
    
    procedure Send_Result_To_Reducer(Cursor : Ada_Mr.Helper.String_String_Maps.Cursor) is
      Reducer_Identifier : String := Ada_Mr.Helper.String_String_Maps.Key(Cursor);
      Result             : String := Ada_Mr.Helper.String_String_Maps.Element(Reducer_Result_Map, Reducer_Identifier);
    begin
      Ada_Mr.Logger.Put_Line("Sending " & Result & " to " & Reducer_Identifier, Ada_Mr.Logger.Info);
      
      -- Asking master for reducer details
      declare
        Xml_Command : String := Ada_Mr.Xml.Helper.Xml_Command(
          G_T           => Ada_Mr.Xml.Helper.Mapper,
          Command       => "reducer_details",
          Access_Token  => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN"),
          Details       => "<identifier>" & Reducer_Identifier & "</identifier>"
        );
        
        Response : String := Ada_Mr.Helper.Send(
          Master_Ip,
          Master_Port,
          Xml_Command,
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
          Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
        );
        
        Reducer_Details_Xml : Ada_Mr.Xml.Node_Access;
      begin
        Reducer_Details_Xml := Ada_Mr.Xml.Helper.Get_Verified_Content(Ada_Mr.Xml.Parser.Parse(Content => Response));
        
        if Ada_Mr.Xml.Helper.Is_Command(Reducer_Details_Xml, "reducer_details") then
          
          declare
            Xml_Command : String := Ada_Mr.Xml.Helper.Xml_Command(
              G_T     => Ada_Mr.Xml.Helper.Mapper, 
              Command => "job_result",
              Details => Result
            );
            
            Reducer_Ip   : String := Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Reducer_Details_Xml, "details"), "ip");
            Reducer_Port : String := Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Reducer_Details_Xml, "details"), "port");
            
            Response : String := Ada_Mr.Helper.Send(
              Reducer_Ip,
              Reducer_Port,
              Xml_Command,
              Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
              Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
            );
          begin
            Ada_Mr.Logger.Put_Line("Job result send to reducer """ & Reducer_Identifier & """", Ada_Mr.Logger.Info);
          end;
        
        elsif Ada_Mr.Xml.Helper.Is_Command(Reducer_Details_Xml, "error") then
          Ada_Mr.Logger.Put_Line(Ada_Mr.Xml.Get_Value(Ada_Mr.Xml.Find_Child_With_Tag(Reducer_Details_Xml, "details"), "message"), Ada_Mr.Logger.Warn);
          raise Ada_Mr.Mapper.Helper.Reducer_Not_Found;
        end if;
      
      exception
        when Error : others =>
          Ada_Mr.Helper.Print_Exception(Error);
          Ada_Mr.Logger.Put_Line("Reducer not found or not reachable! Sending job result to the master!", Ada_Mr.Logger.Err);
            
          begin
            declare
              Response : String := Ada_Mr.Helper.Send(
                Master_Ip,
                Master_Port,
                Ada_Mr.Xml.Helper.Xml_Command(
                  G_T     => Ada_Mr.Xml.Helper.Mapper,
                  Command => "not_delivered_map_result",
                  Access_Token => Ada_Mr.Helper.Read_Configuration("ACCESS_TOKEN"),
                  Details => "<reducer>" & Reducer_Identifier & "</reducer><result>" & Result & "</result>"
                ),
                Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "MAX_CONNECTION_TRIES")),
                Natural'Value(Ada_Mr.Helper.Read_Configuration("SETTINGS", "TIMEOUT_CONNECTION_TRIES"))
              );
            begin
              null;
            end;
          exception
            when Error : others =>
              Ada_Mr.Helper.Print_Exception(Error);
              Ada_Mr.Logger.Put_Line("Master unreachable! Job failed!", Ada_Mr.Logger.Err);
          end;
      end;
      
    end Send_Result_To_Reducer;
    
  begin
    Master_Ip   := GNAT.Sockets.Inet_Addr(Ada_Mr.Helper.Read_Configuration("MASTER-IP"));
    Master_Port := GNAT.Sockets.Port_Type'Value(Ada_Mr.Helper.Read_Configuration("MASTER-PORT"));
    
    Reducer_Result_Map.Iterate(Send_Result_To_Reducer'Access);
  end Send_Result;
end Ada_Mr.Mapper.Helper;