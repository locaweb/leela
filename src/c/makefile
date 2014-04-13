LEELA_VERSION = $(shell ../scripts/read-version.sh)

CFLAGS    = -Wall

SRC_FILES = $(wildcard src/leela/*.c)
OBJ_FILES = $(subst .c,.o,$(SRC_FILES))

build: libleela.so.$(LEELA_VERSION)

clean:
	rm -f $(OBJ_FILES)
	rm -f try_leela
	rm -f libleela.so*
	rm -f src/driver

$(OBJ_FILES): override CPPFLAGS += -D_POSIX_SOURCE=200112 -D_XOPEN_SOURCE
$(OBJ_FILES): override CFLAGS += -Isrc -std=c99 -pedantic -fPIC -pthread

libleela.so.$(LEELA_VERSION): $(OBJ_FILES)
	$(CC) $(LDFLAGS) -pthread -shared -Wl,-soname,$(@) -o$(@) $(OBJ_FILES) $(LDLIBS) -lzmq
	ln -s -f $(@) libleela.so

src/driver: $(OBJ_FILES)
src/driver: override CPPFLAGS += -D_POSIX_SOURCE=200112 -D_XOPEN_SOURCE
src/driver: override CFLAGS += -Wall -g -Isrc -std=c99 -pedantic -fPIC -pthread
src/driver: override LDLIBS += -lpthread -lzmq
