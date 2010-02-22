-------------------------------------------------------------------------------
-- <STRONG>Copyright &copy; 2009, 2010 by Jan-Hendrik H&uuml;hne.</STRONG>
-- <BLOCKQUOTE>
--   This program is free software; you can redistribute it and/or
--   modify it under the terms of the GNU General Public License as
--   published by the Free Software Foundation; either version 2 of the
--   License, or (at your option) any later version.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--   General Public License for more details.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   You should have received a copy of the GNU General Public License
--   along with this program; if not, write to the Free Software
--   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
--   02111-1307, USA.
-- </BLOCKQUOTE>
-- <BLOCKQUOTE>
--   As a special exception, if other files instantiate generics from
--   this unit, or you link this unit with other files to produce an
--   executable, this unit does not by itself cause the resulting
--   executable to be covered by the GNU General Public License. This
--   exception does not however invalidate any other reasons why the
--   executable file might be covered by the GNU Public License.
-- </BLOCKQUOTE>
--
--  <AUTHOR>
--    Bauhaus-University Weimar<br />
--    Jan-Hendrik H&uuml;hne <jan.huehne@uni-weimar.de>
--  </AUTHOR>
--
--  <PURPOSE>
--    Helper package for the Ada_Mr.Master component. Provides a couple 
--    of helper methods.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada_Mr.Xml;
with GNAT.Sockets;
with Ada_Mr.Helper;
with Ada.Calendar;

package Ada_Mr.Master.Helper is

  package ASU renames Ada.Strings.Unbounded;
  
  
  
  
  type Job_State is (
    Pending,
    In_Progress,
    Done,
    Failed
  );
  
  function To_String(Arg : Job_State) return String;
  -- Returns a job state as a string
  
  function From_String(Arg : String) return Job_State;
  -- Returns a job state from a string
  
  
  
  
  type Worker_Record is record
    Identifier   : ASU.Unbounded_String;
    W_Type       : Ada_Mr.Helper.Worker_Type;
    Ip           : GNAT.Sockets.Inet_Addr_Type;
    Port         : GNAT.Sockets.Port_Type;
    Access_Token : String(1..32);
    Updated_At   : Ada.Calendar.Time;
  end record;
  
  type Worker_Record_Access is access Worker_Record;
  
  package Worker_Entry_Vectors is new Ada.Containers.Vectors(
    Element_Type => Worker_Record_Access,
    Index_Type => Positive
  );
  
  
  
  
  type Not_Delivered_Map_Result is record
    Reducer   : ASU.Unbounded_String;
    Result    : ASU.Unbounded_String;
  end record;
  
  type Not_Delivered_Map_Result_Access is access Not_Delivered_Map_Result;
  
  package Not_Delivered_Map_Result_Vectors is new Ada.Containers.Vectors(
    Element_Type => Not_Delivered_Map_Result_Access,
    Index_Type => Positive
  );
  
  protected Not_Delivered_Map_Results is
    procedure Add(Reducer : String; Result : String);
    -- Procedure to add a new not delivered mapper result
      
    function Get_All_By_Identifier(Identifier : String) return Not_Delivered_Map_Result_Vectors.Vector;
    -- Returns a vector with all not delivered mapper results for a given worker
      
    function Is_Empty return Boolean;
    -- Returns true if no not delivered mapper results available
  end;
  
  Not_Delivered_Map_Results_Vector : Not_Delivered_Map_Result_Vectors.Vector;
  
  
  
  
  protected Aborted is
    procedure Set_Abort;
    -- Procedure to abort the complete system
    
    procedure Set_Exit;
    -- Procedure to exit the complete system
      
    function Get_Abort return Boolean;
    -- Returns the abort status
    
    function Get_Exit return Boolean;
    -- Returns the exit status
  private
    Abort_Master  : Boolean := false;
    Exit_Master   : Boolean := false;
  end Aborted;
  
  
  
  
  No_Job_Found : Exception;
  Initialization_Required : Exception;
  No_Worker_Found : Exception;
  Unknown_Job_State : Exception;
  
  
  Stop_Map_Reduce_System : Boolean := False;
  
end Ada_Mr.Master.Helper;