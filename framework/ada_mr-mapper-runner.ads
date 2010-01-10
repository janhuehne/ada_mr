with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Xml;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;
with Ada_Mr.Generics.Runner;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Compute_Job(Job : in My_Job);
  with function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  with procedure Stop_Mapper;
    
package Ada_Mr.Mapper.Runner is  
  
  package ASU renames Ada.Strings.Unbounded;
  
  procedure Run;
  
  package Runner is new Ada_Mr.Generics.Runner(
    Run
  );

end Ada_Mr.Mapper.Runner;
