LEELA_VERSION = $(shell ../scripts/read-version.sh)

CFLAGS = -O2 -Wall -pedantic

SRC_FILES = $(wildcard src/leela/*.c)
OBJ_FILES = $(subst .c,.o,$(SRC_FILES))

build: libleela.so.$(LEELA_VERSION)

clean:
	rm -f $(OBJ_FILES)
	rm -f try_leela
	rm -f libleela.so*
	rm -f src/driver

$(OBJ_FILES): override CPPFLAGS += -D_POSIX_SOURCE
$(OBJ_FILES): override CFLAGS += -Isrc -std=c99 -fPIC -pthread -fstack-protector

libleela.so.$(LEELA_VERSION): $(OBJ_FILES)
	$(CC) $(LDFLAGS) -pthread -shared -Wl,-soname,$(@) -o$(@) $(OBJ_FILES) $(LDLIBS) -lzmq
	ln -s -f $(@) libleela.so

src/driver: $(OBJ_FILES)
src/driver: override CPPFLAGS += -D_POSIX_SOURCE
src/driver: override CFLAGS += -g -Isrc -std=c99 -fPIC -pthread -fstack-protector
src/driver: override LDLIBS += -lpthread -lzmq
