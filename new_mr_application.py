#! /usr/bin/env python
import os

# readin workspace
default_workspace = '~/ada_mr'
user_workspace = raw_input('Workspace path [' + default_workspace + ']: ')

if user_workspace == "":
  user_workspace = default_workspace


# readin project name
project_path = ""
while(project_path == ""):
  project_path = raw_input('Project path (folder where the project files stored): ')


#readin package name
default_package_name = "Demo_Job"
user_package_name = raw_input('Package name (only "Job" is not allowed) [' + default_package_name + ']: ')

if user_package_name == "":
  user_package_name = default_package_name

create_example_config_files = ""
while(create_example_config_files != "y" and create_example_config_files != "n"):
  create_example_config_files = raw_input('Create example config files, too? [YyNn]: ').lower()


# merge path
path = os.path.expanduser(user_workspace)  + "/" + project_path + "/"

# create project folder
os.makedirs(path)

# job.ads
job_ads = ("""\
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.

-- As a special exception, if other files instantiate generics from
-- this unit, or you link this unit with other files to produce an
-- executable, this unit does not by itself cause the resulting
-- executable to be covered by the GNU General Public License. This
-- exception does not however invalidate any other reasons why the
-- executable file might be covered by the GNU Public License.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada_Mr.Helper;
with Ada_Mr.Xml;
with Ada_Mr.Job;

package #job_package# is
  
  -- Package rename
  package ASU renames Ada.Strings.Unbounded;
  
  
  -- Job record definition
  type Job is new Ada_Mr.Job.Object with record
    null;
  end record;
  
  
  -- xml stuff
  overriding function To_Xml(The_Job : Job) return String;
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Job;
  
  
  -- split and get raw data
  procedure Split_Raw_Data;
  overriding function Get_Next_Raw_Job return Job;
  
  
  -- print job on stdio (optional)
  -- overriding procedure Print_Job(The_Job : Job; State : String; Message : String);
  
  
  -- compute job (map function)
  overriding procedure Compute_Job(The_Job : Job);
  
  
  -- splitting the job result for serval reducers
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map;
  
  
  -- merge job results
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean);
  
  
  -- handle the final reducer results
  procedure Finalize;
  
  
  
  
  -- package instance
  -- Vector to store all computed jobs
  package Job_Vector is new Ada.Containers.Vectors(
    Element_Type => Job, 
    Index_Type => Positive
  );
  
  
  -- Precalculated jobs
  Calculated_Jobs : Job_Vector.Vector;
  
end #job_package#;
""")

job_ads = job_ads.replace("#job_package#", user_package_name.title())

# create job.ads
file_name = user_package_name + ".ads"
out_file = open(path + file_name,"w")
out_file.write(job_ads)
out_file.close()




# job.adb
job_adb = ("""\
with Ada.Text_IO;
with Ada_Mr.Logger;
with Ada_Mr.Xml.Helper;


package body #job_package# is
  
  overriding function To_Xml(The_Job : Job) return String is
    Details : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    Details.Insert("job_id", Ada_Mr.Helper.Trim(The_Job.Job_Id'Img));
    
    -- Example:
    -- Details.Insert("key, "value");
    
    return Ada_Mr.Xml.Helper.Hash_To_Xml_String(Details);
  end To_Xml;
  
  
  overriding function From_Xml(Xml_Node : Ada_Mr.Xml.Node_Access) return Job is
    J : Job;
  begin
    J.Job_Id := Integer'Value(Ada_Mr.Xml.Get_Value(Xml_Node, "job_id"));
    
    return J;
  end From_Xml;
  
  
  procedure Split_Raw_Data is
  begin
    declare
      The_Job : Job;
    begin
      Calculated_Jobs.Append(The_Job);
    end;
  end Split_Raw_Data;
  
  
  overriding function Get_Next_Raw_Job return Job is
    J : Job := Calculated_Jobs.First_Element;
  begin
    Calculated_Jobs.Delete_First;
    
    return J;
  end Get_Next_Raw_Job;
  
  
--  overriding procedure Print_Job(The_Job : Job; State : String; Message : String) is
--  begin
--    Ada_Mr.Helper.Put(The_Job.Job_Id'Img, 10, 1);
--    Ada_Mr.Helper.Put("Please override ""Print_Job"" for more details", 50, 1);
--    Ada_Mr.Helper.Put(State, 20);
--    Ada_Mr.Helper.Put(Message, 20);
--    Ada.Text_IO.New_Line;
--  end Print_Job;
  
  
  overriding procedure Compute_Job(The_Job : Job) is
  begin
    null;
  end Compute_Job;
  
  
  function Split_Result_For_Different_Reducer return Ada_Mr.Helper.String_String_Maps.Map is
    Mapping  : Ada_Mr.Helper.String_String_Maps.Map;
  begin
    -- Example:
    --  Mapping.Insert(
    --    "Reducer_1", 
    --    ""
    --  );
    
    -- !! Dont't forget to reset the mapper result after submitting it !!
    return Mapping;
  end Split_Result_For_Different_Reducer;
  
  
  procedure Merge_Job_Results(Xml_Node : Ada_Mr.Xml.Node_Access; Stop_System : out Boolean) is
  begin
    null;
    
    -- Never stop the system at this point
    Stop_System := false;
  end Merge_Job_Results;
  
  
  procedure Finalize is
  begin
    null;
  end Finalize;
  
end #job_package#;
""")

job_adb = job_adb.replace("#job_package#", user_package_name.title())

# create job.adb
file_name = user_package_name + ".adb"
out_file = open(path + file_name,"w")
out_file.write(job_adb)
out_file.close()




# make file
makefile = ("""\
VER=1.0.0

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-largs -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -mmacosx-version-min=10.5.0 -shared-libgcc -ladacrypt -ladamr -ladacrypt
LIBS=-I/usr/local/lib/libadamr -I/usr/local/lib/libadacrypt

all: clean pure_master pure_mapper pure_reducer clean_compilation_files

pure_master:
	$(DEL) master
	$(CC) $(LIBS) master.adb $(CFLAGS)

pure_mapper:
	$(DEL) mapper
	$(CC) $(LIBS) mapper.adb $(CFLAGS)

pure_reducer:
	$(DEL) reducer
	$(CC) $(LIBS) reducer.adb $(CFLAGS)

clean_compilation_files:
	$(DEL) *.o *.ali


master: pure_master clean_compilation_files

mapper: pure_mapper clean_compilation_files

reducer: pure_reducer clean_compilation_files

clean:
	$(DEL) *.o *.ali master mapper reducer
""")

# create Makefile
file_name = "Makefile"
out_file = open(path + file_name,"w")
out_file.write(makefile)
out_file.close()




# master.adb
master_adb = ("""\
-- insert specification for the job
with Ada_Mr.Master.Main;

procedure Master is
  package Job renames #job_package#;
  
  package Master_MR is new Ada_Mr.Master.Main(
    Job.Job,
    Job.From_Xml,
    Job.To_Xml,
    Job.Set_Job_Id,
    Job.Get_Job_Id,
    Job.Print_Job,
    Job.Split_Raw_Data,
    Job.Get_Next_Raw_Job
  );
begin
  
  declare
    M : Master_MR.Master_Task_Access := new Master_MR.Master_Task;
  begin
    M.Start(M);
  end;

end Master;
""")

master_adb = master_adb.replace("#job_package#", user_package_name.title())
master_adb = master_adb.replace("-- insert specification for the job", "with " + user_package_name.title() + ";")

# create master.adb
file_name = "master.adb"
out_file = open(path + file_name,"w")
out_file.write(master_adb)
out_file.close()




#mapper.adb
mapper_adb = ("""\
-- insert specification for the job
with Ada_Mr.Mapper.Main;

procedure Mapper is
  package Job renames #job_package#;
    
  package Mapper_MR is new Ada_Mr.Mapper.Main(
    Job.Job,
    Job.From_Xml,
    Job.To_Xml,
    Job.Get_Job_Id,
    Job.Compute_Job,
    Job.Split_Result_For_Different_Reducer
  );
begin

  declare
    C   : Mapper_MR.Mapper_Task_Access := new Mapper_MR.Mapper_Task;
  begin
    C.Start(C);
  end;
  
end Mapper;
""")

mapper_adb = mapper_adb.replace("#job_package#", user_package_name.title())
mapper_adb = mapper_adb.replace("-- insert specification for the job", "with " + user_package_name.title() + ";")

# create mapper.adb
file_name = "mapper.adb"
out_file = open(path + file_name,"w")
out_file.write(mapper_adb)
out_file.close()




reducer_adb = ("""\
-- insert specification for the job
with Ada_Mr.Reducer.Main;

procedure Reducer is
  package Job renames #job_package#;
  
  package Reducer_MR is new Ada_Mr.Reducer.Main(
    Job.Merge_Job_Results, 
    Job.Finalize
  );
  
begin
  
  declare
    C : Reducer_MR.Reducer_Task_Access := new Reducer_MR.Reducer_Task;
  begin
    C.Start(C);
  end;
  
end Reducer;
""")

reducer_adb = reducer_adb.replace("#job_package#", user_package_name.title())
reducer_adb = reducer_adb.replace("-- insert specification for the job", "with " + user_package_name.title() + ";")

# create reducer.adb
file_name = "reducer.adb"
out_file = open(path + file_name,"w")
out_file.write(reducer_adb)
out_file.close()




# master_config
master_config = ("""\
<?xml version="1.0" ?>
<adamr-master-config>
  
  <local_server>
    <ip></ip>
    <port></port>
  </local_server>
  
  
  <crypto>
    <hmac></hmac>
  </crypto>
  
  
  <settings>
    <max_connection_tries></max_connection_tries>
    <timeout_connection_tries></timeout_connection_tries>
    <log_level></log_level>
  </settings>
  
  
  <user></user>
  
</adamr-master-config>
""")

# create master_config.xml
if create_example_config_files == "y":
  file_name = "master_config.xml.example"
  out_file = open(path + file_name,"w")
  out_file.write(master_config)
  out_file.close()




# mapper_config
mapper_config = ("""\
<?xml version="1.0" ?>
<adamr-mapper-config>
  
  <identifier></identifier>
  
  
  <local_server>
    <ip></ip>
    <port></port>
  </local_server>
  
  
  <master>
    <ip></ip>
    <port></port>
  </master>
  
  
  <crypto>
    <hmac></hmac>
  </crypto>
  
  
  <settings>
    <max_connection_tries></max_connection_tries>
    <timeout_connection_tries></timeout_connection_tries>
    <log_level></log_level>
  </settings>
  
  
  <user></user>
  
</adamr-mapper-config>
""")

# create mapper_config.xml
if create_example_config_files == "y":
  file_name = "mapper_config.xml.example"
  out_file = open(path + file_name,"w")
  out_file.write(mapper_config)
  out_file.close()




# reducer_config
reducer_config = ("""\
<?xml version="1.0" ?>
<adamr-reducer-config>
  
  <identifier></identifier>
  
  
  <local_server>
    <ip></ip>
    <port></port>
  </local_server>
  
  
  <master>
    <ip></ip>
    <port></port>
  </master>
  
  
  <crypto>
    <hmac></hmac>
  </crypto>
  
  
  <settings>
    <max_connection_tries></max_connection_tries>
    <timeout_connection_tries></timeout_connection_tries>
    <log_level></log_level>
  </settings>
  
  
  <user></user>
  
</adamr-reducer-config>
""")

# create reducer_config.xml
if create_example_config_files == "y":
  file_name = "reducer_config.xml.example"
  out_file = open(path + file_name,"w")
  out_file.write(reducer_config)
  out_file.close()