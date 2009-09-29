pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b~test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b~test.adb");

with System.Restrictions;

package body ada_main is
   pragma Warnings (Off);

   procedure Do_Finalize;
   pragma Import (C, Do_Finalize, "system__standard_library__adafinal");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   procedure adainit is
      E017 : Boolean; pragma Import (Ada, E017, "system__secondary_stack_E");
      E013 : Boolean; pragma Import (Ada, E013, "system__soft_links_E");
      E023 : Boolean; pragma Import (Ada, E023, "system__exception_table_E");
      E070 : Boolean; pragma Import (Ada, E070, "ada__io_exceptions_E");
      E077 : Boolean; pragma Import (Ada, E077, "ada__strings_E");
      E048 : Boolean; pragma Import (Ada, E048, "ada__tags_E");
      E046 : Boolean; pragma Import (Ada, E046, "ada__streams_E");
      E105 : Boolean; pragma Import (Ada, E105, "interfaces__c_E");
      E063 : Boolean; pragma Import (Ada, E063, "system__finalization_root_E");
      E083 : Boolean; pragma Import (Ada, E083, "ada__strings__maps_E");
      E101 : Boolean; pragma Import (Ada, E101, "ada__strings__maps__constants_E");
      E065 : Boolean; pragma Import (Ada, E065, "system__finalization_implementation_E");
      E061 : Boolean; pragma Import (Ada, E061, "ada__finalization_E");
      E073 : Boolean; pragma Import (Ada, E073, "ada__finalization__list_controller_E");
      E079 : Boolean; pragma Import (Ada, E079, "ada__strings__unbounded_E");
      E071 : Boolean; pragma Import (Ada, E071, "system__file_control_block_E");
      E059 : Boolean; pragma Import (Ada, E059, "system__file_io_E");
      E103 : Boolean; pragma Import (Ada, E103, "ada__wide_text_io_E");
      E006 : Boolean; pragma Import (Ada, E006, "ada__text_io_E");
      E098 : Boolean; pragma Import (Ada, E098, "simple_xml__xml_io_E");
      E075 : Boolean; pragma Import (Ada, E075, "xml_E");
      E095 : Boolean; pragma Import (Ada, E095, "xml_parser_E");

      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Zero_Cost_Exceptions : Integer;
      pragma Import (C, Zero_Cost_Exceptions, "__gl_zero_cost_exceptions");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");

      procedure Install_Handler;
      pragma Import (C, Install_Handler, "__gnat_install_handler");

      Handler_Installed : Integer;
      pragma Import (C, Handler_Installed, "__gnat_handler_installed");
   begin
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False),
         Value => (0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, True, True, False, False, False, True, 
           True, True, False, False, False, False, False, True, 
           True, False, True, True, True, True, True, True, 
           True, False, True, False, True, False, True, False, 
           False, True, False, False, False, True, False, True, 
           False, False, False, False, False, False, False, True, 
           True, True, False, False, False, True, True, False, 
           True, True, True, False, False, False, False, False, 
           False, False),
         Count => (0, 0, 0, 0, 0, 0, 0),
         Unknown => (False, False, False, False, False, False, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Zero_Cost_Exceptions := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      if Handler_Installed = 0 then
         Install_Handler;
      end if;

      System.Soft_Links'Elab_Body;
      E013 := True;
      System.Secondary_Stack'Elab_Body;
      E017 := True;
      System.Exception_Table'Elab_Body;
      E023 := True;
      Ada.Io_Exceptions'Elab_Spec;
      E070 := True;
      Ada.Strings'Elab_Spec;
      E077 := True;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E046 := True;
      Interfaces.C'Elab_Spec;
      E105 := True;
      System.Finalization_Root'Elab_Spec;
      E063 := True;
      Ada.Strings.Maps'Elab_Spec;
      E083 := True;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E101 := True;
      System.Finalization_Implementation'Elab_Spec;
      System.Finalization_Implementation'Elab_Body;
      E065 := True;
      Ada.Finalization'Elab_Spec;
      E061 := True;
      Ada.Finalization.List_Controller'Elab_Spec;
      E073 := True;
      Ada.Strings.Unbounded'Elab_Spec;
      E079 := True;
      System.File_Control_Block'Elab_Spec;
      E071 := True;
      System.File_Io'Elab_Body;
      E059 := True;
      Ada.Wide_Text_Io'Elab_Spec;
      Ada.Tags'Elab_Body;
      E048 := True;
      Ada.Wide_Text_Io'Elab_Body;
      E103 := True;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E006 := True;
      Simple_Xml.Xml_Io'Elab_Spec;
      E098 := True;
      Xml'Elab_Spec;
      E075 := True;
      E095 := True;
   end adainit;

   procedure adafinal is
   begin
      Do_Finalize;
   end adafinal;

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure initialize (Addr : System.Address);
      pragma Import (C, initialize, "__gnat_initialize");

      procedure finalize;
      pragma Import (C, finalize, "__gnat_finalize");

      procedure Ada_Main_Program;
      pragma Import (Ada, Ada_Main_Program, "_ada_test");

      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Break_Start;
      Ada_Main_Program;
      Do_Finalize;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   ./simple_xml.o
   --   ./simple_xml-xml_io.o
   --   ./xml.o
   --   ./xml_parser.o
   --   ./test.o
   --   -L./
   --   -L/usr/local/ada-4.3/lib/gcc/i686-apple-darwin8/4.3.0/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
