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