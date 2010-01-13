with GNAT.Sockets;
with Ada_Mr.Generics.Echo;
with Ada_Mr.Helper;
with Ada_Mr.Xml;

generic
   with function Exit_Server return Boolean;
   with procedure Process_Request(S : GNAT.Sockets.Stream_Access; From : Ada_Mr.Helper.Worker_Type; Xml_Root : Ada_Mr.Xml.Node_Access);
--   with procedure Stop_Main_Task;
     
package Ada_Mr.Generics.Server is
  
  package Echo is new Ada_Mr.Generics.Echo(Process_Request);
  
  task type Server_Task is
    entry Start(Host : GNAT.Sockets.Inet_Addr_Type; Port : GNAT.Sockets.Port_Type);
    entry Stop;
  end Server_Task;
  
end Ada_Mr.Generics.Server;