with GNAT.Sockets;
use GNAT.Sockets;
with Xml;
with Ada.Strings.Unbounded;

with Generic_Runner;

generic
  type My_Job is private;
  with function From_Xml(Xml_Node : Xml.Node_Access) return My_Job;
  with function To_Xml(Job : in My_Job) return String;
  with function Get_Job_Id(Job : in My_Job) return Natural;
  with procedure Compute_Job(Job : in My_Job);
  with function Job_Result_To_Xml return String;
    
package Mapper_Runner is  
  
  package ASU renames Ada.Strings.Unbounded;
  
  procedure Run;
  
  package Runner is new Generic_Runner(
    Run
  );

end Mapper_Runner;
