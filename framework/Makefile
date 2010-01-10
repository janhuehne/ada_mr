VER=1.0.0
LIBDIR=/usr/local/lib
INSTDIR=/usr/local/lib/libadamr-dev-$(VER)

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-c  -shared -gnato -s -O3 -gnatn
LIBS=-Ilib -Imaster -Imapper -Ireducer -Ilibadacrypt

all : make.o

make.o :
	$(CC) $(LIBS) $(CFLAGS) make.ads

.PHONY : clean
.SILENT : clean

clean:
	$(DEL) *.o *.ali

install-shared-os-x :
	mkdir -p $(INSTDIR)
	mkdir -p $(LIBDIR)
	cp *.o *.ali $(INSTDIR)
	cp *.ads $(INSTDIR)