with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash;

package Xml_Helper is
  
  type Group_Tag is (Mapper, Reducer, Master);
  
  package String_String_Maps is new Ada.Containers.Indefinite_Hashed_Maps(
    Key_Type        => String,
    Element_Type    => String,
    Hash            => Ada.Strings.Hash,
    Equivalent_Keys => "="
  );
  
  function To_String(G_T : Group_Tag) return String;
  
  
  function Xml_Command(G_T : Group_Tag; Command : String; Details : String := "") return String;
  function Xml_Command(G_T : Group_Tag; Command : String; Details : String_String_Maps.Map) return String;
  
  function Create_Initialization(G_T : Group_Tag; Identifier : String) return String;
  function Create_Job_Request return String;
  function Create_System_Control(G_T : Group_Tag; Message : String) return String;
  
  
  
  
end Xml_Helper;