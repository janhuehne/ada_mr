VER=1.0.0

PROJECT=pi

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-largs -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -mmacosx-version-min=10.5.0 -shared-libgcc
LIBS=-I../../lib/simple_xml -I../../framework -aL/usr/local/lib/libadacrypt-dev-0.1.0 -aI/usr/local/lib/libadacrypt-dev-0.1.0 -aO/usr/local/lib/libadacrypt-dev-0.1.0
#LIBS=-aL/usr/local/lib/libadamr-dev-1.0.0 -aI/usr/local/lib/libadamr-dev-1.0.0 -aO/usr/local/lib/libadamr-dev-1.0.0



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
