srcroot       = $(CURDIR)

LEELA_VERSION = $(shell ../scripts/read-version.sh)

CFLAGS        = -Wall

SRC_FILES     = $(wildcard src/leela/*.c)
OBJ_FILES     = $(subst .c,.o,$(SRC_FILES))

poly1305aes.a = $(srcroot)/poly1305aes-20050218/poly1305aes.a

build: $(poly1305aes.a) libleela.so.$(LEELA_VERSION)

clean: cleanobj
	rm -r -f `dirname $(poly1305aes.a)`
	rm -r -f src/leela/poly1305aes

cleanobj:
	rm -f $(OBJ_FILES)
	rm -f try_leela
	rm -f libleela.so*
	rm -f src/driver

$(poly1305aes.a):
	mkdir -p src/leela/poly1305aes
	tar -x -z -f ../../lib/poly1305aes-20050218.tar.gz
	$(MAKE) CC="$(CC) $(CFLAGS) -fPIC" -C $(@D)
	cd $(@D) && cp `cat FILES.lib` poly1305aes.h $(srcroot)/src/leela/poly1305aes

$(OBJ_FILES): override CPPFLAGS += -D_POSIX_SOURCE=200112 -D_XOPEN_SOURCE
$(OBJ_FILES): override CFLAGS += -Isrc -std=c99 -pedantic -fPIC -pthread

libleela.so.$(LEELA_VERSION): $(OBJ_FILES)
	$(CC) $(LDFLAGS) -pthread -shared -Wl,-soname,$(@) -o$(@) $(OBJ_FILES) $(LDLIBS) $(poly1305aes.a) -lzmq
	ln -s -f $(@) libleela.so

src/driver: $(OBJ_FILES)
src/driver: override CPPFLAGS += -D_POSIX_SOURCE=200112 -D_XOPEN_SOURCE
src/driver: override CFLAGS += -Wall -g -Isrc -std=c99 -pedantic -fPIC -pthread
src/driver: override LDLIBS += -lpthread -lzmq
