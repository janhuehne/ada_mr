VER=1.0.0

PROJECT=count_char

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-largs -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -mmacosx-version-min=10.5.0 -shared-libgcc -ladacrypt -ladamr -ladacrypt
LIBS=-I/usr/local/lib/libadamr -I/usr/local/lib/libadacrypt


master:
	$(CC) $(LIBS) master_$(PROJECT).adb $(CFLAGS)
	$(DEL) *.o *.ali

mapper:
	$(CC) $(LIBS) mapper_$(PROJECT).adb $(CFLAGS)
	$(DEL) *.o *.ali
	
reducer:
	$(CC) $(LIBS) reducer_$(PROJECT).adb $(CFLAGS)
	$(DEL) *.o *.ali

clean:
	$(DEL) *.o *.ali master_$(PROJECT) mapper_$(PROJECT) reducer_$(PROJECT)
