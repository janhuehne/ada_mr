VER=1.0.0

DEL=rm -f
DELDIR= rm -rf

CC=gnatmake
CFLAGS=-largs -L/Developer/SDKs/MacOSX10.5.sdk/usr/lib -mmacosx-version-min=10.5.0 -shared-libgcc -ladacrypt -ladamr -ladacrypt
LIBS=-I/usr/local/lib/libadamr -I/usr/local/lib/libadacrypt

all: md5_collision_search_test

md5_collision_search_test:
	$(DEL) md5_collision_search_test
	$(CC) $(LIBS) md5_collision_search_test $(CFLAGS)

clean:
	$(DEL) *.o *.ali md5_collision_search_test