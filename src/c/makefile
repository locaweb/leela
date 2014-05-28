poly1305aes.srcroot  = $(CURDIR)/../../lib/poly1305aes
srcroot              = $(CURDIR)

LEELA_LIB_VERSION   := $(shell env component=.leela-lib patch=0 minor=0 ../scripts/read-version.sh)
LEELA_VERSION       := $(shell env component=.leela-lib ../scripts/read-version.sh)

SRC_FILES            = $(wildcard src/leela/*.c)
OBJ_FILES            = $(subst .c,.o,$(SRC_FILES))

CFLAGS               = -Wall

build: poly1305aes.build libleela.so.$(LEELA_LIB_VERSION)

clean: cleanobj poly1305aes.clean

cleanobj:
	rm -f $(OBJ_FILES)
	rm -f try_leela
	rm -f libleela.so*
	rm -f src/driver

$(OBJ_FILES): override CPPFLAGS += -D_POSIX_SOURCE=200112 -D_XOPEN_SOURCE
$(OBJ_FILES): override CFLAGS += -Isrc -std=c99 -pedantic -fPIC -pthread -I$(poly1305aes.h)

libleela.so.$(LEELA_LIB_VERSION): $(OBJ_FILES)
	$(CC) $(LDFLAGS) -pthread -shared -Wl,-soname,$(@) -o$(@) $(OBJ_FILES) $(LDLIBS) $(poly1305aes.a) -lzmq
	[ "$(LEELA_LIB_VERSION)" = "$(LEELA_VERSION)" ] || ln -s -f $(@) libleela.so.$(LEELA_VERSION)
	ln -s -f libleela.so.$(LEELA_VERSION) libleela.so

src/driver: $(OBJ_FILES)
src/driver: override CPPFLAGS += -D_POSIX_SOURCE=200112 -D_XOPEN_SOURCE
src/driver: override CFLAGS += -Wall -g -Isrc -std=c99 -pedantic -fPIC -pthread -I$(poly1305aes.h)
src/driver: override LDLIBS += -lpthread -lzmq -lm

include $(poly1305aes.srcroot)/makefile
