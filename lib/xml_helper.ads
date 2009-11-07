with Utility;
with Ada.Strings.Unbounded;

package Xml_Helper is
  
  package ASU renames Ada.Strings.Unbounded;
  
  type Group_Tag is (Mapper, Reducer, Master);
  
  function To_String(G_T : Group_Tag) return String;
  
  function Is_Valid_Xml_String(Str : String) return Boolean;
  
  function Xml_Command(G_T : Group_Tag; Command : String; Details : String := "") return String;
  function Xml_Command(G_T : Group_Tag; Command : String; Details : Utility.String_String_Maps.Map) return String;
  
  function Create_Initialization(G_T : Group_Tag; Identifier : String) return String;
  function Create_Job_Request return String;
  function Create_System_Control(G_T : Group_Tag; Message : String) return String;
  
  function Hash_To_Xml_String(Details : Utility.String_String_Maps.Map) return String;
  
  
  
end Xml_Helper;