pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 4.3.0 20070903 (experimental) [trunk revision 128061]";
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_test" & Ascii.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure Break_Start;
   pragma Import (C, Break_Start, "__gnat_break_start");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#c27b4f47#;
   u00002 : constant Version_32 := 16#c2cc8187#;
   u00003 : constant Version_32 := 16#77b911ee#;
   u00004 : constant Version_32 := 16#9c7dd3ea#;
   u00005 : constant Version_32 := 16#ec71e7ed#;
   u00006 : constant Version_32 := 16#4285f8a3#;
   u00007 : constant Version_32 := 16#21f490cd#;
   u00008 : constant Version_32 := 16#cd68620c#;
   u00009 : constant Version_32 := 16#b10233a8#;
   u00010 : constant Version_32 := 16#6a252e02#;
   u00011 : constant Version_32 := 16#a70c8a9a#;
   u00012 : constant Version_32 := 16#1342001d#;
   u00013 : constant Version_32 := 16#7daae1aa#;
   u00014 : constant Version_32 := 16#4c0302b0#;
   u00015 : constant Version_32 := 16#5422dc73#;
   u00016 : constant Version_32 := 16#9eabae82#;
   u00017 : constant Version_32 := 16#59a5f902#;
   u00018 : constant Version_32 := 16#7f6459fe#;
   u00019 : constant Version_32 := 16#82e3940e#;
   u00020 : constant Version_32 := 16#36281ef1#;
   u00021 : constant Version_32 := 16#8503f904#;
   u00022 : constant Version_32 := 16#423fa2a0#;
   u00023 : constant Version_32 := 16#601e13e1#;
   u00024 : constant Version_32 := 16#726beeed#;
   u00025 : constant Version_32 := 16#3294453e#;
   u00026 : constant Version_32 := 16#8cef510c#;
   u00027 : constant Version_32 := 16#7ba9a527#;
   u00028 : constant Version_32 := 16#63c7c118#;
   u00029 : constant Version_32 := 16#35eb138e#;
   u00030 : constant Version_32 := 16#373bd87b#;
   u00031 : constant Version_32 := 16#b3c0d16c#;
   u00032 : constant Version_32 := 16#1e1b7442#;
   u00033 : constant Version_32 := 16#1ce48f24#;
   u00034 : constant Version_32 := 16#d0b303ec#;
   u00035 : constant Version_32 := 16#fa27edb6#;
   u00036 : constant Version_32 := 16#021b840c#;
   u00037 : constant Version_32 := 16#ef1ec252#;
   u00038 : constant Version_32 := 16#6963ad4c#;
   u00039 : constant Version_32 := 16#53743e56#;
   u00040 : constant Version_32 := 16#712c642a#;
   u00041 : constant Version_32 := 16#a69cad5c#;
   u00042 : constant Version_32 := 16#7d84fb54#;
   u00043 : constant Version_32 := 16#f48f2a0d#;
   u00044 : constant Version_32 := 16#264aa8fc#;
   u00045 : constant Version_32 := 16#3e5c4df1#;
   u00046 : constant Version_32 := 16#a8d17654#;
   u00047 : constant Version_32 := 16#64af6456#;
   u00048 : constant Version_32 := 16#7af7d3a4#;
   u00049 : constant Version_32 := 16#d33b9bc8#;
   u00050 : constant Version_32 := 16#318c4ed2#;
   u00051 : constant Version_32 := 16#843e8c6d#;
   u00052 : constant Version_32 := 16#fd22f7f6#;
   u00053 : constant Version_32 := 16#b7140ae3#;
   u00054 : constant Version_32 := 16#30bba161#;
   u00055 : constant Version_32 := 16#62e56d2b#;
   u00056 : constant Version_32 := 16#a8e5b34e#;
   u00057 : constant Version_32 := 16#bbbba6c8#;
   u00058 : constant Version_32 := 16#4f9dd01a#;
   u00059 : constant Version_32 := 16#f23d69c6#;
   u00060 : constant Version_32 := 16#fcec4850#;
   u00061 : constant Version_32 := 16#16dfe486#;
   u00062 : constant Version_32 := 16#6d0998e1#;
   u00063 : constant Version_32 := 16#d6484b07#;
   u00064 : constant Version_32 := 16#8e7005dc#;
   u00065 : constant Version_32 := 16#534204e6#;
   u00066 : constant Version_32 := 16#293ff6f7#;
   u00067 : constant Version_32 := 16#bfadf501#;
   u00068 : constant Version_32 := 16#716a9db2#;
   u00069 : constant Version_32 := 16#f7ac1f46#;
   u00070 : constant Version_32 := 16#2274d34a#;
   u00071 : constant Version_32 := 16#166fb0d3#;
   u00072 : constant Version_32 := 16#923573c8#;
   u00073 : constant Version_32 := 16#183b4446#;
   u00074 : constant Version_32 := 16#7c3da751#;
   u00075 : constant Version_32 := 16#9d5724f1#;
   u00076 : constant Version_32 := 16#e74826b3#;
   u00077 : constant Version_32 := 16#1bc9f0e1#;
   u00078 : constant Version_32 := 16#dc80ce83#;
   u00079 : constant Version_32 := 16#bb19d785#;
   u00080 : constant Version_32 := 16#70f768a2#;
   u00081 : constant Version_32 := 16#f0ddc3f6#;
   u00082 : constant Version_32 := 16#eccbd1ca#;
   u00083 : constant Version_32 := 16#e59f2e92#;
   u00084 : constant Version_32 := 16#48e1b0c3#;
   u00085 : constant Version_32 := 16#04e247f8#;
   u00086 : constant Version_32 := 16#cc1134cf#;
   u00087 : constant Version_32 := 16#60144c8b#;
   u00088 : constant Version_32 := 16#3ab3e7b1#;
   u00089 : constant Version_32 := 16#54ed61ee#;
   u00090 : constant Version_32 := 16#41dcc4f6#;
   u00091 : constant Version_32 := 16#88595bf5#;
   u00092 : constant Version_32 := 16#c5845840#;
   u00093 : constant Version_32 := 16#df067dd7#;
   u00094 : constant Version_32 := 16#a689b1f3#;
   u00095 : constant Version_32 := 16#9661218e#;
   u00096 : constant Version_32 := 16#28fc5492#;
   u00097 : constant Version_32 := 16#2e2199fc#;
   u00098 : constant Version_32 := 16#55c2735e#;
   u00099 : constant Version_32 := 16#69f72c24#;
   u00100 : constant Version_32 := 16#8cddb9b3#;
   u00101 : constant Version_32 := 16#35ecf6c7#;
   u00102 : constant Version_32 := 16#ec4fbfe0#;
   u00103 : constant Version_32 := 16#6b1208ed#;
   u00104 : constant Version_32 := 16#59507545#;
   u00105 : constant Version_32 := 16#e98c0dd7#;
   u00106 : constant Version_32 := 16#56af4987#;
   u00107 : constant Version_32 := 16#c4c92782#;
   u00108 : constant Version_32 := 16#4c526528#;
   u00109 : constant Version_32 := 16#7bab78e1#;
   u00110 : constant Version_32 := 16#9caa1f36#;
   u00111 : constant Version_32 := 16#94b4fcb4#;

   pragma Export (C, u00001, "testB");
   pragma Export (C, u00002, "system__standard_libraryB");
   pragma Export (C, u00003, "system__standard_libraryS");
   pragma Export (C, u00004, "adaS");
   pragma Export (C, u00005, "ada__text_ioB");
   pragma Export (C, u00006, "ada__text_ioS");
   pragma Export (C, u00007, "ada__exceptionsB");
   pragma Export (C, u00008, "ada__exceptionsS");
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   pragma Export (C, u00011, "systemS");
   pragma Export (C, u00012, "system__soft_linksB");
   pragma Export (C, u00013, "system__soft_linksS");
   pragma Export (C, u00014, "system__parametersB");
   pragma Export (C, u00015, "system__parametersS");
   pragma Export (C, u00016, "system__secondary_stackB");
   pragma Export (C, u00017, "system__secondary_stackS");
   pragma Export (C, u00018, "system__storage_elementsB");
   pragma Export (C, u00019, "system__storage_elementsS");
   pragma Export (C, u00020, "system__stack_checkingB");
   pragma Export (C, u00021, "system__stack_checkingS");
   pragma Export (C, u00022, "system__exception_tableB");
   pragma Export (C, u00023, "system__exception_tableS");
   pragma Export (C, u00024, "system__htableB");
   pragma Export (C, u00025, "system__htableS");
   pragma Export (C, u00026, "system__exceptionsB");
   pragma Export (C, u00027, "system__exceptionsS");
   pragma Export (C, u00028, "system__string_opsB");
   pragma Export (C, u00029, "system__string_opsS");
   pragma Export (C, u00030, "system__string_ops_concat_3B");
   pragma Export (C, u00031, "system__string_ops_concat_3S");
   pragma Export (C, u00032, "system__tracebackB");
   pragma Export (C, u00033, "system__tracebackS");
   pragma Export (C, u00034, "system__unsigned_typesS");
   pragma Export (C, u00035, "system__wch_conB");
   pragma Export (C, u00036, "system__wch_conS");
   pragma Export (C, u00037, "system__wch_stwB");
   pragma Export (C, u00038, "system__wch_stwS");
   pragma Export (C, u00039, "system__wch_cnvB");
   pragma Export (C, u00040, "system__wch_cnvS");
   pragma Export (C, u00041, "interfacesS");
   pragma Export (C, u00042, "system__wch_jisB");
   pragma Export (C, u00043, "system__wch_jisS");
   pragma Export (C, u00044, "system__traceback_entriesB");
   pragma Export (C, u00045, "system__traceback_entriesS");
   pragma Export (C, u00046, "ada__streamsS");
   pragma Export (C, u00047, "ada__tagsB");
   pragma Export (C, u00048, "ada__tagsS");
   pragma Export (C, u00049, "system__val_unsB");
   pragma Export (C, u00050, "system__val_unsS");
   pragma Export (C, u00051, "system__val_utilB");
   pragma Export (C, u00052, "system__val_utilS");
   pragma Export (C, u00053, "system__case_utilB");
   pragma Export (C, u00054, "system__case_utilS");
   pragma Export (C, u00055, "interfaces__c_streamsB");
   pragma Export (C, u00056, "interfaces__c_streamsS");
   pragma Export (C, u00057, "system__crtlS");
   pragma Export (C, u00058, "system__file_ioB");
   pragma Export (C, u00059, "system__file_ioS");
   pragma Export (C, u00060, "ada__finalizationB");
   pragma Export (C, u00061, "ada__finalizationS");
   pragma Export (C, u00062, "system__finalization_rootB");
   pragma Export (C, u00063, "system__finalization_rootS");
   pragma Export (C, u00064, "system__finalization_implementationB");
   pragma Export (C, u00065, "system__finalization_implementationS");
   pragma Export (C, u00066, "system__restrictionsB");
   pragma Export (C, u00067, "system__restrictionsS");
   pragma Export (C, u00068, "system__stream_attributesB");
   pragma Export (C, u00069, "system__stream_attributesS");
   pragma Export (C, u00070, "ada__io_exceptionsS");
   pragma Export (C, u00071, "system__file_control_blockS");
   pragma Export (C, u00072, "ada__finalization__list_controllerB");
   pragma Export (C, u00073, "ada__finalization__list_controllerS");
   pragma Export (C, u00074, "xmlB");
   pragma Export (C, u00075, "xmlS");
   pragma Export (C, u00076, "ada__containersS");
   pragma Export (C, u00077, "ada__stringsS");
   pragma Export (C, u00078, "ada__strings__unboundedB");
   pragma Export (C, u00079, "ada__strings__unboundedS");
   pragma Export (C, u00080, "ada__strings__fixedB");
   pragma Export (C, u00081, "ada__strings__fixedS");
   pragma Export (C, u00082, "ada__strings__mapsB");
   pragma Export (C, u00083, "ada__strings__mapsS");
   pragma Export (C, u00084, "system__bit_opsB");
   pragma Export (C, u00085, "system__bit_opsS");
   pragma Export (C, u00086, "ada__charactersS");
   pragma Export (C, u00087, "ada__characters__latin_1S");
   pragma Export (C, u00088, "ada__strings__searchB");
   pragma Export (C, u00089, "ada__strings__searchS");
   pragma Export (C, u00090, "system__compare_array_unsigned_8B");
   pragma Export (C, u00091, "system__compare_array_unsigned_8S");
   pragma Export (C, u00092, "system__address_operationsB");
   pragma Export (C, u00093, "system__address_operationsS");
   pragma Export (C, u00094, "xml_parserB");
   pragma Export (C, u00095, "xml_parserS");
   pragma Export (C, u00096, "simple_xmlS");
   pragma Export (C, u00097, "simple_xml__xml_ioB");
   pragma Export (C, u00098, "simple_xml__xml_ioS");
   pragma Export (C, u00099, "ada__characters__handlingB");
   pragma Export (C, u00100, "ada__characters__handlingS");
   pragma Export (C, u00101, "ada__strings__maps__constantsS");
   pragma Export (C, u00102, "ada__wide_text_ioB");
   pragma Export (C, u00103, "ada__wide_text_ioS");
   pragma Export (C, u00104, "interfaces__cB");
   pragma Export (C, u00105, "interfaces__cS");
   pragma Export (C, u00106, "system__string_ops_concat_5B");
   pragma Export (C, u00107, "system__string_ops_concat_5S");
   pragma Export (C, u00108, "system__string_ops_concat_4B");
   pragma Export (C, u00109, "system__string_ops_concat_4S");
   pragma Export (C, u00110, "system__memoryB");
   pragma Export (C, u00111, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.handling%s
   --  ada.characters.latin_1%s
   --  ada.containers%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.bit_ops%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.compare_array_unsigned_8%s
   --  system.htable%s
   --  system.htable%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.restrictions%s
   --  system.restrictions%b
   --  system.standard_library%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.compare_array_unsigned_8%b
   --  system.secondary_stack%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_ops%s
   --  system.string_ops%b
   --  system.string_ops_concat_3%s
   --  system.string_ops_concat_3%b
   --  system.string_ops_concat_4%s
   --  system.string_ops_concat_4%b
   --  system.string_ops_concat_5%s
   --  system.string_ops_concat_5%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  ada.exceptions.last_chance_handler%s
   --  system.soft_links%s
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.secondary_stack%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.tags%s
   --  ada.streams%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.unsigned_types%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.fixed%s
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%b
   --  system.stream_attributes%s
   --  system.stream_attributes%b
   --  system.finalization_implementation%s
   --  system.finalization_implementation%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  ada.finalization.list_controller%s
   --  ada.finalization.list_controller%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_uns%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_uns%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  ada.wide_text_io%s
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.tags%b
   --  ada.exceptions%b
   --  ada.wide_text_io%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  simple_xml%s
   --  simple_xml.xml_io%s
   --  simple_xml.xml_io%b
   --  xml%s
   --  xml%b
   --  xml_parser%s
   --  xml_parser%b
   --  test%b
   --  END ELABORATION ORDER

end ada_main;
