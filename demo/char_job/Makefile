VER=1.0.0

PROJECT=count_char

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-largs -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -mmacosx-version-min=10.5.0 -shared-libgcc
LIBS=-I../lib/simple_xml -I../framework -aL/usr/local/lib/libadacrypt-dev-0.1.0 -aI/usr/local/lib/libadacrypt-dev-0.1.0 -aO/usr/local/lib/libadacrypt-dev-0.1.0
#LIBS=-aL/usr/local/lib/libadamr-dev-1.0.0 -aI/usr/local/lib/libadamr-dev-1.0.0 -aO/usr/local/lib/libadamr-dev-1.0.0



master:
	$(CC) -I../master $(LIBS) mr_master_$(PROJECT).adb $(CFLAGS)
	$(DEL) *.o *.ali

mapper:
	$(CC) -I../mapper $(LIBS) mr_mapper_$(PROJECT).adb $(CFLAGS)
	$(DEL) *.o *.ali
	
reducer:
	$(CC) $(LIBS) mr_reducer_$(PROJECT).adb $(CFLAGS)
	$(DEL) *.o *.ali

clean:
	$(DEL) *.o *.ali mr_master_$(PROJECT) mr_mapper_$(PROJECT) mr_reducer_$(PROJECT)
