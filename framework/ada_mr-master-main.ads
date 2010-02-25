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
--    Provides the main package for the Ada_Mr.Master component.
--  </PURPOSE>
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada_Mr.Xml;
with Ada_Mr.Master.Helper;
with Ada_Mr.Master.Server;
with Ada_Mr.Generics.Console;
with Ada_Mr.Generics.Runner;
with Ada_Mr.Helper;

generic
  type My_Job is private;
  with function From_Xml
    (Xml_Node : Ada_Mr.Xml.Node_Access)
    return My_Job;
  with function To_Xml
    (Job : in My_Job)
    return String;
  with procedure Set_Job_Id
    (The_Job : in out My_Job);
  with function Get_Job_Id
    (Job : in My_Job)
    return Natural;
  with procedure Print_Job
    (Job : in My_Job; 
     State : String; 
     Message : String);
  with procedure Split_Raw_Data;
  with function Get_Next_Raw_Job
    return My_Job;
  
package Ada_Mr.Master.Main is
  
  package ASU renames Ada.Strings.Unbounded;
  
  
  
  
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => My_Job, 
    Index_Type => Positive
  );
  
  
  
  
  type Master_Task;
  type Master_Task_Access is access Master_Task;
  
  task type Master_Task is
    entry Start(Self : Master_Task_Access);
    entry Stop;
  end Master_Task;
  
  
  procedure Stop_Master_Task;
  -- Stops the master task.
  
  
  
  
  function Exit_Observer return Boolean;
  -- Returns <code>True</code> when the observer should terminate.
  
  procedure Observe;
  -- Observes serval variables and states.
  
  package Observer is new Ada_Mr.Generics.Runner(
    Observe
  );
  
  type My_Job_Access is access My_Job;
  
  
  
  
  type Job_Entry_Record is record
    Job     : My_Job;
    State   : Ada_Mr.Master.Helper.Job_State := Ada_Mr.Master.Helper.Pending;
    Message : ASU.Unbounded_String := ASU.To_Unbounded_String("");
  end record;
  
  type Job_Entry_Record_Access is access Job_Entry_Record;
  
  function "="
    (Left, Right : Job_Entry_Record_Access)
    return Boolean;
  -- Returns <code>True</code> if <code>left = right</code>.
  
  
  package Job_Entry_Record_Vectors is new Ada.Containers.Vectors(
    Element_Type => Job_Entry_Record_Access,
    Index_Type => Positive,
    "=" => "="
  );
  
  
  function Job_Entry_To_Xml
    (Job_Entry : Job_Entry_Record_Access)
    return String;
  -- Converts a <code>Job_Entry</code> into a xml string.
  
  
  procedure Change_Job_State
    (Job_Entry : in out Job_Entry_Record_Access; 
     State     : Ada_Mr.Master.Helper.Job_State;
     Message   : String := "");
  -- Changes the state of a job and sets a message.
  
  
  
  
  protected Jobs is
    procedure Add
      (Job : My_Job);
    -- Adds a new job.
    
    function Get_By_Id(Id : Natural) return Job_Entry_Record_Access;
    -- Returns a job specified by the <code>Id</code>.
      
    function Get_Next_Pending return Job_Entry_Record_Access;
    -- Returns a job with state <code>Pending</code>.
    
    function Count return Natural;
    -- Counts all jobs and returns the results.
    
    function Count_By_State(State : Ada_Mr.Master.Helper.Job_State) return Natural;
    -- Counts all jobs with a specific state and returns the result.
    
    procedure Print;
    -- Prints a job on STD/IO.
  private
    Jobs : Job_Entry_Record_Vectors.Vector;
  end Jobs;
  
  
  
----------------------------------------------------
-- PROTECTED TYPE TO HANDLE JOBS                   -
----------------------------------------------------
  protected Worker is
    procedure Add
      (New_Worker : in out Ada_Mr.Master.Helper.Worker_Record_Access);
    -- Adds a new worker.
    
    procedure Stop_All;
    -- Stop all connected workers.
    
    function Exists_Identifier
      (Identifier : String)
      return Boolean;
    -- Returns <code>True</code> if a worker with the <code>Identifier</code> is connected.
      
    function Find_By_Identifier
      (Identifier : String) 
      return Ada_Mr.Master.Helper.Worker_Record_Access;
    -- Returns the worker with the <code>Identifier</code>.
    
    function Find_By_Access_Token_And_Type
      (Access_Token : String; 
       W_Type       : Ada_Mr.Helper.Worker_Type)
      return Ada_Mr.Master.Helper.Worker_Record_Access;
    -- Returns the worker with the specified <code>access_token</code> and the worker type.
    
    function Find_All_By_Type
      (W_Type : Ada_Mr.Helper.Worker_Type)
      return Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector;
    -- Returns a vector with all workers matched the given worker type.
    
    procedure Print;
    -- Prints the worker details on STD/IO.
  private
    Worker : Ada_Mr.Master.Helper.Worker_Entry_Vectors.Vector;
    
    Mapper_Counter  : Natural := 1;
    Reducer_Counter : Natural := 1;
  end Worker;
  
  
  
  
  package Server is new Ada_Mr.Master.Server(
    My_Job,
    Job_Entry_Record_Access,
    From_Xml,
    Worker.Add,
    Worker.Find_By_Identifier,
    Worker.Find_By_Access_Token_And_Type,
    Jobs.Add,
    Jobs.Get_By_Id,
    Jobs.Get_Next_Pending,
    Change_Job_State,
    Job_Entry_To_Xml,
    Stop_Master_Task
  );
  
  
  
  
  function Banner return String;
  -- Returns the name of this component
  
  procedure Process_User_Input
    (User_Input  : String; 
     To_Controll : Master_Task_Access);
  -- Processes the user inputs
  
  package Console is new Ada_Mr.Generics.Console(
    Master_Task_Access,
    Banner,
    Process_User_Input
  );
  
private
  Main_Task : Master_Task_Access;
end Ada_Mr.Master.Main;