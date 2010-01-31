VER=1.0.0
LIBDIR=/usr/local/lib
INSTDIR=/usr/local/lib/libadamr-dev-$(VER)

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-c -O3
LIBS=-I/usr/local/lib/libadacrypt -ladacrypt

all : make.o
share-and-install : clean make.o shared install-shared

make.o :
	$(CC) $(LIBS) $(CFLAGS) make.ads

.PHONY : clean
.SILENT : clean

clean:
	$(DEL) *.o *.ali

shared:
	ar rc libadamr.a.$(VER) ada_mr*.o simple_xml*.o

install-shared:
	$(DELDIR) $(LIBDIR)/libadamr*
	mkdir -p $(INSTDIR)
	mkdir -p $(LIBDIR)
	cp *.ads *.adb *.ali $(INSTDIR)
	chmod 444 $(INSTDIR)/*
	ln -s $(INSTDIR) $(LIBDIR)/libadamr
	cp libadamr.a.$(VER) $(LIBDIR)/
	ln -s $(LIBDIR)/libadamr.a.$(VER) $(LIBDIR)/libadamr.a
	ldconfig

uninstall-shared:
	$(DELDIR) $(LIBDIR)/libadamr*
	ldconfig
