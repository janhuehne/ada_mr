with GNAT.Sockets;

generic
   with function Exit_Server return Boolean;
   with procedure Process_Incomming_Connection(New_Sock : GNAT.Sockets.Socket_Type);
   with procedure Stop_Main_Task;
     
package Ada_Mr.Generics.Server is
  
  task type Server_Task is
    entry Start(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type);
    entry Stop;
  end Server_Task;
  
end Ada_Mr.Generics.Server;