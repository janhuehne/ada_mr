------------------------------------------------------------------------------
--                                                                          --
--      JR-ADA-Tools - Ada 95 general Development Framework and Tools       --
--                                                                          --
--             Copyright (C) 2004, Jerome Raffalli                          --
--                                                                          --
--   This package is a free adaptation of GtkAda's Glib.XML                 --
--     Copyright (C) Emmanuel Briot, Joel Brobecker and Arnaud Charlet      --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

-- <SUMMARY> -----------------------------------------------------------------
-- XML_Io is the String version of Simple XML Parser and Writer
-- Wide_XML_Io is the Wide_String version of the Simple XML Parser and Writer
--
-- <DESCRIPTION> -------------------------------------------------------------
-- This package is (and should stay) a simple Xml parser and writer
--   * No memory node tree (less memory and cpu overhead)
--   * Easy to use
--   * No validation
--   * Support for String and Wide_String
--   * File or string content support
--
--  for more complex Xml needs, full Xml suites like Xml-Ada should be considered
--
-- <DEVELOPMENT> -------------------------------------------------------------
--   Public revision : 1.0
--   $Id: jio-Ada_Mr.Xml.ads,v 1.1 2003/01/13 12:12:05 jerome Exp $
--   Notes :
------------------------------------------------------------------------------
with Interfaces.C_Streams;
with Interfaces.C;
with Ada.Characters.Handling;
with Ada.Wide_Text_Io;
with Ada.Strings;
with Ada.Strings.Fixed;
with Unchecked_Deallocation;
with System.WCh_Con;
--with System.WCh_Cnv;
with System.WCh_StW;
with System.Crtl;

package body Simple_Xml.Xml_Io is

  use Interfaces.C_Streams;
  use Ada.Strings;
  use Ada.Characters.Handling;

--  type Character_Mode is (Char_Mode, Wide_Char_Mode);
--  Mode : constant Character_Mode := Char_Mode;
  Char_Size : constant := Character'size /8;

-----------------------------------------------------------------------------------
--- Utilities region --------------------------------------------------------------
-----------------------------------------------------------------------------------

  -- UTF-8 <==> UNICODE
  function  UTF8_To_String  (Value : String) return String is
    use Ada.Characters.Handling;
    use System.WCh_StW;
    
    R : Wide_String(1..100);
    L : Natural;
  begin
    String_To_Wide_String(Value, R, L, System.Wch_Con.WCEM_UTF8);
    return To_String (R(1..L));
  end UTF8_To_String;

  function  Wide_UTF8_To_Wide_String  (Value : Wide_String) return Wide_String is
    use Ada.Characters.Handling;
    use System.WCh_StW;
    
    R : Wide_String(1..100);
    L : Natural;
  begin
    String_To_Wide_String (To_String (Value), R, L, System.Wch_Con.WCEM_UTF8);
    return R(1..L);
  end Wide_UTF8_To_Wide_String;

   -- String insensitive comparison
   function Like (Left, Right : String) return Boolean is
     use Ada.Strings.Fixed;

     T_Left  : constant String := Trim (Left, Both);
     T_Right : constant String := Trim (Right, Both);

     function Like (Left, Right : Character) return Boolean is
     begin
       if Left = Right
       then
         return True;
       end if;
       case Left is
       when 'a' .. 'z'|'A' .. 'Z' =>
         return (Abs (Character'pos (Left) - Character'pos (Right)) = 16#20#);
       when others =>
         return False;
       end case;
     end Like;

   begin

     if T_Left'length /= T_Right'length
     then
       return False;
     end if;

     for I in 0 .. T_Left'length-1
     loop
       if not Like (T_Left (T_Left'first +I), T_Right (T_Right'first +I))
       then
         return False;
       end if;
     end loop;

     return True;

   end Like;

  -- Pt_String basic utilities
  procedure Free is new Unchecked_Deallocation (String, Pt_String);

  function To_Pt_String (Source : String) return Pt_String is
  begin
    if Source /= ""
    then
      return new String'(Source);
    else
      return null;
    end if;
  end To_Pt_String;
  function Length       (Source : Pt_String) return Integer is
  begin
    if Source /= null
    then
      return Source.all'length;
    else
      return 0;
    end if;
  end Length;
  function To_String    (Source : Pt_String) return String is
  begin
    if Source /= null
    then
      return Source.all;
    else
      return "";
    end if;
  end To_String;
  procedure Set (Source : in out Pt_String; To_Value : in String) is
  begin
    Free(Source);
    Source := To_Pt_String(To_Value);
  end Set;

  function Attribute (List : Xml_Attributes; Name : in String) return String is
  begin
    for I in List'range
    loop
      if Like (To_String (List(I).Name), Name)
      then
        return To_String(List(I).Value);
      end if;
    end loop;
    return "";
  end Attribute;

  function Attribute (Name : String; Value : String) return Xml_Attribute is
  begin
    return (To_Unbounded_String(Name), To_Unbounded_String(Value));
  end Attribute;

  -- Returns the name or value of an attribute
  function Attribute_Name  (Attribute : Xml_Attribute) return String is
  begin
    return To_String(Attribute.Name);
  end Attribute_Name;
  function Attribute_Value (Attribute : Xml_Attribute) return String is
  begin
    return To_String(Attribute.Value);
  end Attribute_Value;

  function Translate (S : String) return String is
  begin
    for I in S'Range
    loop
      case S (I) is
      when '<'    => return S(S'first .. I-1) & "&lt;" & Translate(S(I+1 .. S'last));
      when '>'    => return S(S'first .. I-1) & "&gt;" & Translate(S(I+1 .. S'last));
      when '&'    => return S(S'first .. I-1) & "&amp;" & Translate(S(I+1 .. S'last));
      when '''    => return S(S'first .. I-1) & "&apos;" & Translate(S(I+1 .. S'last));
      when '"'    => return S(S'first .. I-1) & "&quot;" & Translate(S(I+1 .. S'last)); --"
      when others => null;
      end case;
    end loop;
    return S;
  end Translate;

-----------------------------------------------------------------------------------
--- Parse region ------------------------------------------------------------------
-----------------------------------------------------------------------------------

   --  Parse an Xml content or else a file containing XML
   procedure Parse (File_Name : in String   := "";
                    Content   : in String := "") is

     -- Supress warnings about potential infinite loop in Get_Node
     pragma Warnings (Off);
      
     Is_A_Stream : constant Boolean := Content'length > 0;

     UTF_8    : Boolean := False; -- Received string values encoding
     Convert  : Boolean := False; -- if Auto_Conversion is applied

     Buf      : Pt_String       := null;     -- Parsing buffer
     Index    : Integer         := 0;        -- Current read position in Buf
     Buf_Last : Integer         := 0;        -- Last index of Buf

     procedure Free_Buf is
     begin
       if Is_A_Stream then return;end if;  -- dont free the buf because it points to the passed Content
     end Free_Buf;

     -- Strings are pointers in buf
     type String_Ptr is
     record
       First : Integer := 1;
       Last  : Integer := 0;
     end record;

     type Ty_Node is
     record
       Tag        : String_Ptr;
       Attributes : String_Ptr;
       Value      : String_Ptr;
     end record;

     Char_LF   : constant Character := Character'val (16#0A#);
     Char_HT   : constant Character := Character'val (16#09#);
     Char_CR   : constant Character := Character'val (16#0D#);
     function Is_Blank (Char : Character) return Boolean is
     begin
       case Char is
       when ' ' | Char_LF | Char_HT | Char_CR => return True;
       when others => return False;
       end case;
     end Is_Blank;

     --  Read Buf'length characters in The_File and store it in Buf.
     --  Problem is to read both classic and unicode files from any mode
     --    Classic file is read using fread and Unicode files
     --    are read using Wide_Text_Io
     procedure Load_Entire_File is
       UTF_16         : Boolean := False;
       File_Size      : Long;             -- Lenght of file in bytes
       Name           : aliased constant String := File_Name & Ascii.Nul;
       Read_Mode      : aliased constant String := "rb+" & Ascii.Nul;
       BOM            : aliased String (1 .. 4) := "    ";
       F              : Files;
       Retour         : Int;
       BOM_1          : constant Character      := Character'val (16#FE#);
       BOM_2          : constant Character      := Character'val (16#FF#);
       BOM_3          : constant Character      := Character'val (16#EF#);
     begin
       -- Check file presence
       if File_Exists (Name'address) = 0 then return;end if;

       -- Get size and unicode status
       F         := Fopen (Name'address, Read_Mode'Address);
       Retour    := int (fread(BOM'address, 2 ,1 ,F));   -- Read the BOM
       Retour    := fseek (F, 0, SEEK_END);
       File_Size := ftell (F);
       Retour    := fclose(F);
       case BOM (1) is
       when BOM_1|BOM_2 => UTF_16 := True;
       when BOM_3       => UTF_8  := True;
       when others => null;
       end case;
       if UTF_16   -- Remove 2 bytes
       then
         declare
           File_Buf : aliased Wide_String (1 .. (Integer (System.Crtl."/"(File_Size,2))));
         begin
           F      := Fopen (Name'address, Read_Mode'Address);
           Retour := int (fread(File_Buf'address, size_t (File_Size) ,1 ,F));
           Retour := fclose(F);
           -- (=Wide)  Buf    := new Wide_String '(File_Buf (2 .. File_Buf'length));
             Buf    := new String'(To_String (File_Buf (2 .. File_Buf'length)));
         end;
       else
         declare
           File_Buf : aliased String (1 .. Integer (File_Size));
         begin
           F      := Fopen (Name'address, Read_Mode'Address);
           Retour := int (fread(File_Buf'address, size_t (File_Size) ,1 ,F));
           Retour := fclose(F);
           if UTF_8   -- Remove first 3 bytes
           then
             -- (=Wide)  Buf    := new Wide_String '(To_Wide_String (File_Buf (4 .. File_Buf'length)));
               Buf    := new String '(File_Buf (4 .. File_Buf'length));
           else
             -- (=Wide)  Buf    := new Wide_String '(To_Wide_String (File_Buf));
               Buf    := new String '(File_Buf);
           end if;
         end;
       end if;
     end Load_Entire_File;

     procedure Load_Buffer is
     begin
       
       if Is_A_Stream
       then
         Buf      := To_Pt_String(Content);
       else
         Load_Entire_File;
       end if;
     end Load_Buffer;

     --  Skip blanks, LF and CR, starting at Index. Index is updated to the
     --  new position (first non blank or EOF)
     procedure Skip_Blanks(Index : in out Integer;Last : in Integer) is
     begin
       while Index < Last
         and then Is_Blank(Buf(Index))
       loop
         Index := Index + 1;
       end loop;
     end Skip_Blanks;

     --  On return, S will contain the String starting at Buf (Index) and
     --  terminating before the first 'Terminator' character. Index will also
     --  point to the next non blank character.
     --  The special XML '&' characters are translated appropriately in S.
     procedure Get_Buf
       (Terminator : Character;
        S          : in out String_Ptr) is
     begin
       S.First := Index;
       while Buf (Index) /= Terminator
       loop
         Index := Index + 1;
       end loop;
  
       S.Last := Index - 1;
       Index  := Index + 1;

       Skip_Blanks(Index, Buf_Last);

     end Get_Buf;

     --  Translate S by replacing the XML '&' special characters by the
     --  actual ASCII character.
     --  This function currently handles:
     --   - &quot;
     --   - &gt;
     --   - &lt;
     --   - &amp;
     --   - &apos;
     function Translate (S : String_Ptr) return String is
       Len       : constant Integer := S.Last-S.First+1;
       Str       : String (1 .. Len);
       Start, J  : Positive := 1;
       Index     : Positive := S.First;
       In_String : Boolean  := False;
     begin
       if Len = 0 then return "";end if; -- Nothing to do
 
       loop
         if In_String or else Buf (Index) /= '&'
         then
           Str (J) := Buf (Index);
         else
           Index := Index + 1;
           Start := Index;

           while Buf (Index) /= ';'
           loop
             Index := Index + 1;
           end loop;

           if    Buf (Start .. Index - 1) = "quot"
           then
             Str (J) := '"'; --"
           elsif Buf (Start .. Index - 1) = "gt"
           then
             Str (J) := '>';
           elsif Buf (Start .. Index - 1) = "lt"
           then
             Str (J) := '<';
           elsif Buf (Start .. Index - 1) = "amp"
           then
             Str (J) := '&';
           elsif Buf (Start .. Index - 1) = "apos"
           then
             Str (J) := ''';
           else
             -- keep existing chars
             for I in Start-1 .. Index
             loop
               Str(J) := Buf(I);
               J := J+1;
             end loop;
             J := J-1;
           end if;
         end if;

         exit when Index = S.Last;

         if Buf (Index) = '"' --"
         then
           In_String := not In_String;
         end if;

         Index := Index + 1;
         J     := J + 1;
       end loop;

       if Convert
       then
         return UTF8_To_String (Str (1 .. J));
         -- (=Wide)return Str (1 .. J);
       else
         return Str (1 .. J);
       end if;

     end Translate;

     function Value
       (Ptr : String_Ptr;
        Translated : Boolean := False) return String is
     begin
       if Ptr.First > Ptr.Last
       then
         return "";
       elsif Translated
       then
         return Translate(Ptr);
       else
         return String(Buf(Ptr.First .. Ptr.Last));
       end if;
     end Value;
   
     --  Extract the attributes as a string, if the tag contains blanks ' '
     --  On return, Tag is unchanged and Attributes contains the string
     --  If the last character in Tag is '/' then the node is empty and
     --  Empty_Node is set to True.
     procedure Extract_Attrib
       (Tag        : in out String_Ptr;
        Attributes : out String_Ptr;
        Empty_Node : out Boolean) is

        Index             : Natural := Tag.First;
        Index_Last_Of_Tag : Natural;

     begin

       --  First decide if the node is empty
       Empty_Node := (Buf (Tag.Last) = '/');

       --  Try to detect a blank
       while Index <= Tag.Last
         and then not Is_Blank(Buf (Index))
       loop
         Index := Index + 1;
       end loop;

       Index_Last_Of_Tag := Index - 1;

       Skip_Blanks (Index, Tag.Last);

       if Index <= Tag.Last
       then
         Attributes.First := Index;
         if Empty_Node
         then
           Attributes.Last := Tag.Last -1;
         else
           Attributes.Last := Tag.Last;
         end if;
         Tag.Last := Index_Last_Of_Tag;
       end if;

     end Extract_Attrib;

     function To_Attributes(S : String_Ptr) return Xml_Attributes is
       Nb_Attributes : Integer := 0;
       In_A_Value    : Boolean := False;
     begin
       -- first count the attributes
       for I in S.First .. S.Last
       loop
         if    Buf(I) = '='
         then
           if not In_A_Value
           then
             Nb_Attributes := Nb_Attributes +1;
           end if;
         elsif Buf(I) = '"' --"
         then
           In_A_Value := not In_A_Value;
         end if;
       end loop;

       declare

         List   : Xml_Attributes(1 .. Nb_Attributes);
         Index  : Integer := S.First;

         procedure Get_Name(Attrib : in Integer) is
           Start : Integer := Index;
         begin

           -- Go to next '=' or next ' '
           while Index < S.Last
           loop
             case Buf(Index) is
             when ' '|'=' => exit;
             when others  => null;
             end case;
             Index := Index+1;
           end loop;
           
           -- Store the name
           List(Attrib).Name := To_Unbounded_String(String(Buf(Start .. Index-1)));

         end Get_Name;

         procedure Get_Value(Attrib : in Integer) is
           Result : String_Ptr;
         begin

           -- Go to next "
           while Index < S.Last
             and then Buf(Index) /= '"' --"
           loop
             Index := Index+1;
           end loop;
           Index   := Index+1;
           Result.First := Index;
           
           -- Go to next "
           while Index < S.Last
             and then Buf(Index) /= '"' --"
           loop
             Index := Index+1;
           end loop;
           Result.Last  := Index-1;
           List(Attrib).Value := To_Unbounded_String(Value (Result,Translated => True));

           -- Skip remaining blanks
           Index   := Index+1;
           while Index < S.Last
             and then Buf(Index) = ' '
           loop
             Index := Index+1;
           end loop;

         end Get_Value;

       begin
         for I in 1 .. Nb_Attributes
         loop
           Get_Name(I);
           Get_Value(I);
         end loop;
         return List;
       end;

     end To_Attributes;

     Depth : Integer := 0;

     --  The main parse routine. Starting at Index.all, Index.all is updated
     --  on return. Return the node starting at Buf (Index.all) which will
     --  also contain all the children and subchildren.

     procedure Get_Node is
       Node       : Ty_Node;
       Empty_Node : Boolean;
       Child_Node : Boolean;
     begin

       Index      := Index + 1;

       -- Get the tag name and attributes
       Get_Buf ('>', Node.Tag);

       --  Check to see whether it is a comment, !DOCTYPE, or the like:
       case Buf (Node.Tag.First) is
       when '!'|'?' =>

         -- Treat next node, because this is a comment (or the prolog)
         Get_Node;

       when others  =>

         --  Here we have to deal with the attributes of the form
         --  <tag attrib='xyyzy'>

         Extract_Attrib (Node.Tag, Node.Attributes, Empty_Node);

         --  it is possible to have a child-less node that has the form
         --  <tag /> or <tag attrib='xyyzy'/>

         Child_Node := not Empty_Node and then (Buf (Index) = '<');

         -- Get the value if it is present
         if not Empty_Node
           and then not Child_Node
         then
           --  Get the value of this node
           Get_Buf ('<', Node.Value);
         end if;

         Depth := Depth +1;
         -- Create the new node
         Open_Node(Tag        => Value(Node.Tag),
                   Attributes => To_Attributes(Node.Attributes),
                   Value      => Value(Node.Value,Translated => True),
                   Depth      => Depth);

         if Child_Node
         then

           --  Parse the children nodes
           while Buf (Index + 1) /= '/'
           loop
             Get_Node;
           end loop;

           Index := Index + 1;

         end if;

         -- Skip the node terminator
         if not Empty_Node
         then
           Index := Index + 1;
           declare
             S : String_Ptr;
           begin
             Get_Buf ('>', S);
           end;
         end if;

         -- Signal the end of this node
         Close_Node  (Depth);
         Depth := Depth -1;

       end case;

     end Get_Node;

     procedure Set_Encoding (Encoding : in String) is
     begin
       if Like (Encoding, "UTF-8")
       then
         UTF_8   := True;
       end if;
       Convert := UTF_8 and (Auto_Conversion /= None);
       Register_Encoding (Encoding);
     end Set_Encoding;

   begin
     Load_Buffer;
     
     if Buf = null
     then
       On_Error;
       return;
     end if;

     Index    := Buf.all'first +1;
     Buf_Last := Buf.all'last;

     declare
       Main_Attributes   : String_Ptr;
       Attributes_String : String_Ptr;
       Empty_Node        : Boolean;
     begin
       Get_Buf ('>', Main_Attributes);
       Extract_Attrib (Main_Attributes, Attributes_String, Empty_Node);
       declare
         Attributes : constant Xml_Attributes := To_Attributes(Attributes_String);
       begin
         Register_Version(Attribute (Attributes, "version"));
         Set_Encoding (Attribute (Attributes, "encoding"));
       end;
     end;

     -- Read nodes
     Get_Node;

     Free_Buf;

   exception
     when others => Free_Buf;On_Error;
   end Parse;

-----------------------------------------------------------------------------------
--- End of Parse region -----------------------------------------------------------
-----------------------------------------------------------------------------------


-----------------------------------------------------------------------------------
--- First method writer region ----------------------------------------------------
-----------------------------------------------------------------------------------

   procedure Save  (File_Name : in String;
                    Success   : out Boolean) is

      use Ada.Text_IO;
      File : File_Type;

      procedure Print (S : String) is
      begin
        Put(File,S);
      end Print;

      procedure Print_Line (S : String) is
      begin
        Put_Line(File,S);
      end Print_Line;

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent (Indent : Natural) is
      begin
         Put (File,(1 .. Indent => ' '));
      end Do_Indent;

      ------------------
      -- Print_String --
      ------------------
      procedure Print_String (S : String) is
      begin
        Print(Translate(S));
      end Print_String;

      ----------------
      -- Print_Node --
      ----------------

      function Node_Attributes return String is
        Name : constant String := Attribute_Name;
      begin
        if Name = "" then return "";end if;
        declare
          Value : constant String := Translate(Attribute_Value);
          Next  : constant String := Node_Attributes;
        begin
          if Next = ""
          then
            return Name & "=""" & Value & """";
          else
            return Name & "=""" & Value & """ " & Next;
          end if;
        end;
      end Node_Attributes;

      procedure Print_Node (Indent : Natural) is
        Tag        : constant String := Node_Tag;
        Attributes : constant String := Node_Attributes;
        Value      : constant String := Node_Value;
      begin
        Do_Indent (Indent);
        Print ("<" & Tag);

        if Attributes /= ""
        then
          Print (" " & Attributes);
        end if;

        if Node_Child
        then
          Print_Line (">");
          loop
            Print_Node (Indent + 2);
            exit when not Next_Node;
          end loop;

          Do_Indent (Indent);
          Print_Line ("</" & Tag & ">");

        else
          if Value = ""
          then
            --  The following handles the difference between what you got
            --  when you parsed <tag/> vs. <tag />.
            if Tag (Tag'Last) = '/'
            then
              Print_Line(">");
            else
              Print_Line("/>");
            end if;
          else
            Print (">");
            Print_String (Value);
            Print_Line("</" & Tag & ">");
          end if;
        end if;
      end Print_Node;

   begin
     Success := False;
     Create     (File, Out_File, File_Name);
     Put_Line   (File, "<?xml version=""1.0"" encoding=""" & Encoding & """ standalone=""yes""?>");
     Print_Node (0);
     Close (File);
     Success := True;
   exception
     when others => null;
   end Save;

-----------------------------------------------------------------------------------
--- End of First method writer region ---------------------------------------------
-----------------------------------------------------------------------------------



-----------------------------------------------------------------------------------
--- Second method writer region ---------------------------------------------------
-----------------------------------------------------------------------------------

   -- returns last detected error
   function Last_Error (Writer : Xml_Writer) return Xml_Error is
   begin
     return Writer.Error;
   end Last_Error;

   -- Output to file or to buffer
   procedure Put (Writer   : in out Xml_Writer;
                  Text     : in String;
                  New_Line : in Boolean := False;
                  Indent   : in Boolean := False) is
     use Ada.Text_IO;
   begin
     if Writer.Buffer /= Null_String
     then
       -- No need to Indent or to add new lines in memory
       declare
         New_Index : constant Integer := Writer.Index + Text'length;
       begin
         if New_Index > Length(Writer.Buffer)
         then
           declare
             Old_Buffer : Pt_String := Writer.Buffer;
           begin
             -- Realloc double size buffer
             Writer.Buffer := new String (1 .. 2*New_Index);
             Writer.Buffer (1 .. Writer.Index) := Old_Buffer (1 .. Writer.Index);
             Free(Old_Buffer);
           end;
         end if;
         Writer.Buffer (Writer.Index+1 .. New_Index) := Text;
         Writer.Index := New_Index;
       end;
     else
       if Indent
         and then Writer.Level > 1
       then
         Put (Writer.File, (1 .. Writer.Level-1 => ' '));
       end if;
       if New_Line
       then
         Put_Line (Writer.File, Text);
       else
         Put (Writer.File, Text);
       end if;
     end if;
   end Put;

   -- Create a new writer
   --  if File_Name = "", no file is created and the
   --    Xml content will be returned in a string buffer
   --     (see Content function)
   procedure Create (Writer    : out Xml_Writer;
                     File_Name : in String;
                     Encoding  : in String := Default_Encoding) is
     Is_A_Stream : constant Boolean := File_Name'length = 0;
   begin
     if Writer.Open
     then
       Writer.Error := Already_Open;
       return;
     end if;

     Writer.Open := True;
     if Is_A_Stream
     then
       Writer.Buffer := new String (1 .. 10_000);
       Writer.Index  := 0;
       Writer.Name   := Unbounded.Null_Unbounded_String;
     else
       declare
         use Ada.Text_IO;
       begin
         Create     (Writer.File, Out_File, File_Name);
       exception
         when others => Writer.Error := Invalid_File_Name;return;
       end;
       Writer.Name := Unbounded.To_Unbounded_String (File_Name);
     end if;
     Writer.Error         := No_Error;
     Writer.Level         := 0;
     Writer.Node_Empty(0) := False;
     Put (Writer, "<?xml version=""1.0"" encoding=""" & Encoding & """ standalone=""yes""?>",
          New_Line => True);
   exception
     when others => Writer.Error := Invalid_File_Name;
   end Create;

   -- Append a new node to the writer
   procedure Open_Node (Writer       : in out Xml_Writer;
                        Tag          : in String;
                        Attributes   : in Xml_Attributes := No_Attribute) is
   begin

     if not Writer.Open
     then
       Writer.Error := Not_Open;
       return;
     end if;

     if Writer.Level = Xml_Level'last
     then
       Writer.Error := Too_Deep;
       return;
     end if;
     Writer.Error := No_Error;

     -- Close parent node tag
     if Writer.Node_Empty (Writer.Level)
     then
       Put (Writer, ">", New_Line => True);
       Writer.Node_Empty (Writer.Level) := False;
       Writer.Node_Child (Writer.Level) := True;
     end if;

     Writer.Level := Writer.Level+1;
     Writer.Node_Child (Writer.Level) := False;
     Writer.Node_Empty (Writer.Level) := True;

     -- Memorize the tag
     Writer.Node_Tag (Writer.Level) := To_Unbounded_String(Tag);

     -- Output tag
     Put (Writer, "<" & Tag, Indent => True);

     -- Output attributes
     if Attributes'length > 0
     then
       for I in Attributes'range
       loop
         Put (Writer, " " & To_String(Attributes(I).Name) & "=""" &
            Translate(To_String(Attributes(I).Value)) & """");
       end loop;
     end if;

   end Open_Node;

   -- Declare the node value
   procedure Set_Value (Writer       : in out Xml_Writer;
                        Value        : in String) is
   begin
     if Value = "" then return;end if;
     if not Writer.Open
     then
       Writer.Error := Not_Open;
       return;
     end if;
     Writer.Error := No_Error;

     -- Close parent node tag
     if Writer.Node_Empty (Writer.Level)
     then
       Put (Writer, ">");
       Writer.Node_Empty (Writer.Level) := False;
     end if;

     -- Output the value
     Put (Writer, Translate(Value));

   end Set_Value;

   -- Complete the new node
   procedure Close_Node (Writer      : in out Xml_Writer) is
   begin
     if not Writer.Open
     then
       Writer.Error := Not_Open;
       return;
     end if;
     if Writer.Level = 0
     then
       Writer.Error := Too_Deep;
       return;
     end if;
     Writer.Error := No_Error;

     declare
       Tag : constant String := To_String (Writer.Node_Tag (Writer.Level));
     begin
       if Writer.Node_Empty(Writer.Level)
       then
         --  The following handles the difference between what you got
         --  when you parsed <tag/> vs. <tag />.
         if Tag (Tag'Last) = '/'
         then
           Put (Writer, ">", New_Line => True);
         else
           Put (Writer, "/>", New_Line => True);
         end if;
       else
         if Writer.Node_Child(Writer.Level)
         then
           Put (Writer, "</" & Tag & ">", New_Line => True, Indent => True);
         else
           Put (Writer, "</" & Tag & ">", New_Line => True);
         end if;
       end if;
     end;

     Writer.Level := Writer.Level-1;

   end Close_Node;

   -- Close the file
   procedure Close      (Writer      : in out Xml_Writer) is
   begin
     if not Writer.Open
     then
       Writer.Error := Not_Open;
       return;
     end if;
     Writer.Error := No_Error;
     if Writer.Level /= 0
     then
       Writer.Error := Incomplete_File;
     end if;
     Finalize (Writer);
     Writer.Name := Unbounded.Null_Unbounded_String;
   end Close;

   -- Returns the XML content in a buffer 
   function  Content    (Writer      : Xml_Writer) return String is
   begin
     if Writer.Buffer = Null_String then return "";end if;
     return Writer.Buffer.all (1 .. Writer.Index);
   end Content;

   -- Returns the file name associated with the Xml writer
   function  File_Name (Writer       : Xml_Writer) return String is
   begin
     return To_String (Writer.Name);
   end File_Name;

   -- tells if the content will be put in a real File or an Xml string
   function  Is_A_File (Writer       : Xml_Writer) return Boolean is
   begin
     return (Writer.Buffer = Null_String);
   end Is_A_File;

   procedure Finalize (This : in out Xml_Writer) is
     use Ada.Text_IO;
   begin
     Free (This.Buffer);
     if Is_Open (This.File)
     then
       Close (This.File);
     end if;
     This.Open := False;
   end Finalize;

-----------------------------------------------------------------------------------
--- End of second method writer region --------------------------------------------
-----------------------------------------------------------------------------------

end Simple_Xml.Xml_Io;

