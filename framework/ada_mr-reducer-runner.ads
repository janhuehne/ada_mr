with GNAT.Sockets;
use GNAT.Sockets;
with Ada_Mr.Xml;
with Ada.Strings.Unbounded;

with Ada_Mr.Generics.Runner;

generic
  with procedure Stop_Reducer;

package Ada_Mr.Reducer.Runner is
  
  package ASU renames Ada.Strings.Unbounded;
  
  procedure Run;
  
  package Runner is new Ada_Mr.Generics.Runner(
    Run
  );

end Ada_Mr.Reducer.Runner;
