VER=0.1.0
LIBDIR=/usr/local/lib
INSTDIR=/usr/local/lib/libadacrypt-dev-$(VER)

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-c  -shared -gnato -s -O3 -gnatn 

all : make.o
install-shared : uninstall-shared all install_shared

make.o :
	$(CC) $(CFLAGS) make.ads

.PHONY : clean
.SILENT : clean

install:
	mkdir -p $(INSTDIR)
	mkdir -p $(LIBDIR)
	cp *.ads *.adb *.o $(INSTDIR)/

uninstall:
	$(DELDIR) $(INSTDIR)

clean:
	$(DEL) *.o *.ali 

#SHARED LIB

shared :
	ar rc libadacrypt.a.$(VER) crypto*.o

clean-shared:
	$(DEL) libadacrypt.so.$(VER)

install_shared :
	mkdir -p $(INSTDIR)
	mkdir -p $(LIBDIR)
	cp crypto*.ads crypto*.ali $(INSTDIR)/
	ln -s $(INSTDIR) $(LIBDIR)/libadacrypt
	chmod 444 $(LIBDIR)/libadacrypt/
	cp libadacrypt.a.$(VER) $(LIBDIR)/
	ln -s $(LIBDIR)/libadacrypt.a.$(VER) $(LIBDIR)/libadacrypt.a
	ldconfig

uninstall-shared:
	$(DEL) -rf $(LIBDIR)/libadacrypt*
