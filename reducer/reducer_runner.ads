with GNAT.Sockets;
use GNAT.Sockets;
with Xml;
with Ada.Strings.Unbounded;

with Generic_Runner;

generic
  with procedure Stop_Reducer;

package Reducer_Runner is
  
  package ASU renames Ada.Strings.Unbounded;
  
  procedure Run;
  
  package Runner is new Generic_Runner(
    Run
  );

end Reducer_Runner;
