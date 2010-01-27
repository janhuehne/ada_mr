

CC = gnatmake	
CFLAGS = -gnato -c -I../src  -O3 -gnatn -gnatwrclp

BB = gnatbind
BFLAGS = -shared -aO./ -aO/lib/ -aO../src -I- -x

LL = gnatlink
#LFLAGS = -s -z combreloc -dy  -x -X -O3 -I../src -aO./ -aO../src -lacl
LFLAGS = -s -z combreloc -dy  -x -X -O3 -I../src -aO./ -aO../src
all : acltest.o

acltest.o : 
	$(CC) $(CFLAGS) acltest
	$(BB) $(BFLAGS) acltest.ali
	$(LL) $(LFLAGS) acltest.ali

.PHONY : clean
.SILENT : clean

clean:
	rm -f acltest acltest.o acltest.ali test*.o test*.ali *.ali *.o


