with GNAT.Sockets;

generic
   with function Exit_Server return Boolean;
   with procedure Process_Incomming_Connection(New_Sock : GNAT.Sockets.Socket_Type);
    
package Generic_Server is
  
  task type Server_Task is
    entry Start(Host : String; Port : GNAT.Sockets.Port_Type);
    entry Stop;
  end Server_Task;
  
end Generic_Server;