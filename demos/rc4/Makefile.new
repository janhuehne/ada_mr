CC=gnat compile
CFLAGS=-gnato -O3 
CLIBS=-I../../framework/  -I/usr/local/lib/libadacrypt-dev-0.1.0/ -I../../lib/simple_xml

BIND=gnat bind
BFLAGS=-shared
BLIBS=-aO./  -aO/usr/local/lib/libadacrypt-dev-0.1.0/ -I- -x


LINK=gnat link
LFLAGS=-s -z combreloc -dy  -x -X -O3
LLIBS=-I/usr/local/lib/libadacrypt-dev-0/ -ladacrypt 

all : mapper_rc4.o

mapper_rc4.o:
	$(CC)   $(CFLAGS) $(CLIBS) mapper_rc4.adb
	$(BIND) $(BFLAGS) $(BLIBS) mapper_rc4.ali
	$(LINK) $(LFLAGS) $(LLIBS) mapper_rc4.ali

.PHONY : clean
.SILENT : clean

clean:
	rm -f mapper_rc4 *.o *.ali

