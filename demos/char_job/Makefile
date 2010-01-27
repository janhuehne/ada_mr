VER=1.0.0

PROJECT=count_char

DEL=rm -f
DELDIR= rm -rf

#CC=gnatmake
#CFLAGS=-largs -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -mmacosx-version-min=10.5.0 -shared-libgcc
#LIBS=-I../../lib/simple_xml -I../../framework -aL/usr/local/lib/libadacrypt-dev-0.1.0 -aI/usr/local/lib/libadacrypt-dev-0.1.0 #-aO/usr/local/lib/libadacrypt-dev-0.1.0
#LIBS=-aL/usr/local/lib/libadamr-dev-1.0.0 -aI/usr/local/lib/libadamr-dev-1.0.0 -aO/usr/local/lib/libadamr-dev-1.0.0


CC=gnatmake -c
CFLAGS=-gnato  -O3
CLIBS=-I/usr/local/lib/libadacrypt  -I/usr/local/lib/libadamr -I../../lib/simple_xml

BB=gnatmake -b
BFLAGS=-shared
BLIBS=-aO./ -aO/usr/local/lib/libadacrypt -aO/usr/local/lib/libadamr -I- -x

LL=gnatlink
LFLAGS=#-s -z combreloc -dy  -x -X -O3
LLIBS=-ladacrypt -ladamr -mmacosx-version-min=10.5.0 -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib


mapper:
	$(CC) $(CFLAGS) $(CLIBS) mapper_$(PROJECT).adb
	$(BB) $(BFLAGS) $(BLIBS) mapper_$(PROJECT).ali
	$(LL) $(LFLAGS) $(LLIBS) mapper_$(PROJECT).ali
        
#	$(CC) $(LIBS) master_$(PROJECT).adb $(CFLAGS)
#	$(DEL) *.o *.ali

#mapper:
#	$(CC) $(LIBS) mapper_$(PROJECT).adb $(CFLAGS)
#	$(DEL) *.o *.ali
#	
#reducer:
#	$(CC) $(LIBS) reducer_$(PROJECT).adb $(CFLAGS)
#	$(DEL) *.o *.ali

clean:
	$(DEL) *.o *.ali master_$(PROJECT) mapper_$(PROJECT) reducer_$(PROJECT)
