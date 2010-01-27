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
--  For more complex Xml needs, full Xml suites like Xml-Ada should be considered
--
--  Both String and Wide_String parsers accept 8 bit or 16 bit encoded XML input
--  String writer always generates 8 bit output
--  Wide_String writer always generates 16 bit output
--
-- <DEVELOPMENT> -------------------------------------------------------------
--   Public revision : 1.0
--   $Id: jio-Ada_Mr.Xml.ads,v 1.1 2003/01/13 12:12:05 jerome Exp $
--   Notes :
------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded;

package Simple_Xml.Xml_Io is

   -- Attributes parsing
   type Xml_Attribute is private;
   Null_Attribute : constant Xml_Attribute;
   type Xml_Attributes is array (Integer range <>) of Xml_Attribute;
   No_Attribute : constant Xml_Attributes;

   function Attribute (List : Xml_Attributes;Name : String) return String;
   -- Find an attribute in the list
   --   "" is returned if not found

   function Attribute_Name  (Attribute : Xml_Attribute) return String;
   function Attribute_Value (Attribute : Xml_Attribute) return String;
   -- Returns the name or value of an attribute


   function Attribute (Name : String;Value : String) return Xml_Attribute;
   -- Build an attribute from name and value

   Default_Encoding      : constant  String;

   --------------------------------------------------------------------------
   --   generic Xml parser
   --------------------------------------------------------------------------
   type Char_Conversion is (None, To_Iso_8859_1);

   generic

     with procedure Open_Node (Tag        : in String;
                               Attributes : in Xml_Attributes;
                               Value      : in String;
                               Depth      : in Integer);
     -- Register a new node
     --  Depth is the imbrication level of the node
     --  The first node has a depth of 1

     with procedure Close_Node (Depth      : in Integer);
     -- Complete the new node

     with procedure Register_Version (Version : in String);
     -- Register the version of the file

     with procedure Register_Encoding (Encoding : in String);
     -- Register the encoding

     with procedure On_Error;
     -- Error notification

     Auto_Conversion : Char_Conversion := To_Iso_8859_1;
     -- Additionnal decoding feature
     -- If /= None, a translation is applied to received strings
     --    when possible
     -- The only currently supported translation is from UTF-8 to Iso-8859-1
     --   and will occur only if File is explicitly UTF8 signed

   procedure Parse (File_Name : in String   := "";
                    Content   : in String := "");
   --  Parse an Xml content or else a file containing XML


   --------------------------------------------------------------------------
   --   generic Xml file writer
   --------------------------------------------------------------------------
   generic

     with function  Next_Node return Boolean;
     -- If there is a brother node, point the brother node and return True
     -- If no more brother nodes, point the parent node and return False

     with function  Node_Tag return String;
     -- return the tag of the current (pointed) node

     with function  Attribute_Name return String;
     -- If the next unread attribute is not null, point it and return its name
     -- else return ""

     with function  Attribute_Value return String;
     -- If the current attribute is not null, return its valuer
     -- else return ""

     with function  Node_Value return String;
     -- return the value of the current (pointed) node

     with function  Node_Child return Boolean;
     -- If there is children nodes, point the first child and return True
     -- else return False

     Encoding : String := Default_Encoding;

   procedure Save  (File_Name : in  String;
                    Success   : out Boolean);
   --  Generate an XML File
   --    the root node is assumed to be initialy pointed
   --  No particular character encoding is applied to passed
   --   values. Strings are supposed to be already encoded according
   --    to the 'Encoding' parameter


   --------------------------------------------------------------------------
   --   classic Xml writer
   --      Can put generated XML content in a file or in a string
   --------------------------------------------------------------------------
   type Xml_Writer is limited private;
   type Xml_Error is
     (No_Error,
      Invalid_File_Name,
      Already_Open,
      Not_Open,
      Write_Error,
      Incomplete_File,
      Too_Deep);
   
   function Last_Error (Writer : Xml_Writer) return Xml_Error;
   -- Returns last detected error

   procedure Create (Writer    : out Xml_Writer;
                     File_Name : in String;
                     Encoding  : in String := Default_Encoding);
   -- Create a new writer
   --  if File_Name = "", no file is created and the
   --    Xml content will be returned in a string buffer
   --     (see Content function)
   --  No particular character encoding is applied to passed
   --   values. Strings are supposed to be already encoded according
   --    to the 'Encoding' parameter

   function  File_Name (Writer       : Xml_Writer) return String;
   -- Returns the file name associated with the Xml writer

   function  Is_A_File (Writer       : Xml_Writer) return Boolean;
   -- tells if the content will be put in a real File or an Xml string

   procedure Open_Node (Writer       : in out Xml_Writer;
                        Tag          : in String;
                        Attributes   : in Xml_Attributes := No_Attribute);
   -- Append a new node to the writer

   procedure Set_Value (Writer       : in out Xml_Writer;
                        Value        : in String);
   -- Declare the node value

   procedure Close_Node (Writer      : in out Xml_Writer);
   -- Complete the new node

   procedure Close      (Writer      : in out Xml_Writer);
   -- Close the file

   function  Content    (Writer      : Xml_Writer) return String;
   -- Returns the XML content in Buffer 

private

  use Ada.Strings.Unbounded;
  use Ada.Strings.Unbounded;
  Default_Encoding   : constant String      := "ISO-8859-1";

  type Xml_Attribute is
  record
    Name  : Unbounded_String;                    -- Attribute name
    Value : Unbounded_String;                    -- Attribute value
  end record;
  Null_Attribute  : constant Xml_Attribute := (Null_Unbounded_String, Null_Unbounded_String);
  No_Attribute    : constant Xml_Attributes(1 .. 0) := (others => Null_Attribute);

  type Pt_String is access String;
  Null_String : constant Pt_String := null;
  subtype Xml_Level is Integer range 0 .. 50;
  type Xml_Strings is array (Xml_Level) of Unbounded_String;
  type Xml_Booleans is array (Xml_Level) of Boolean;
  type Xml_Writer is new Ada.Finalization.Limited_Controlled with
  record
    Error      : Xml_Error := No_Error;         -- Last error
    Buffer     : Pt_String := Null_String;      -- Memory file
    Name       : Unbounded_String;              -- File or stream name
    Index      : Integer   := 0;                -- Buffer offset
    File       : Ada.Text_Io.File_Type;  -- Os file
    Open       : Boolean   := False;            -- Open status of the writer
    Level      : Xml_Level := 0;                -- Current node depth
    Node_Tag   : Xml_Strings;                   -- Chain of open nodes
    Node_Child : Xml_Booleans;                  -- Node has children
    Node_Empty : Xml_Booleans;                  -- Node is empty
  end record;

  procedure Finalize (This : in out Xml_Writer);

end Simple_Xml.Xml_Io;
