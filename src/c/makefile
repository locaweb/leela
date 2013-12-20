LEELA_VERSION = $(shell ../scripts/read-version.sh)

CFLAGS = -O2 -Wall -pedantic

SRC_FILES = $(wildcard src/leela/*.c)
OBJ_FILES = $(subst .c,.o,$(SRC_FILES))

build: libleela.so.$(LEELA_VERSION)

try_leela: override CPPFLAGS += -DLEELA_TRACING -pthread
try_leela: override CXXFLAGS += -g -Isrc -Itry -I/usr/include/unittest++
try_leela: override LDFLAGS += -pthread
try_leela: override LDLIBS += -lUnitTest++ -lzmq
try_leela: try/*.cc $(OBJ_FILES)

test: try_leela
	[ -n "$(LEELA_ENDPOINT)" ] || { echo "missing environ variable: LEELA_ENDPOINT (eg.: LEELA_ENDPOINT='tcp://localhost:4080;'"; exit 1; }
	env LEELA_ENDPOINT="$(LEELA_ENDPOINT)" ./try_leela

clean:
	rm -f $(OBJ_FILES)
	rm -f try_leela
	rm -f libleela.so*

$(OBJ_FILES): override CPPFLAGS += -D_POSIX_SOURCE
$(OBJ_FILES): override CFLAGS += -Isrc -std=c99 -fPIC -pthread -fstack-protector

libleela.so.$(LEELA_VERSION): $(OBJ_FILES)
	$(CC) $(LDFLAGS) -pthread -shared -Wl,-soname,$(@) -o$(@) $(OBJ_FILES) $(LDLIBS) -lzmq
	ln -s -f $(@) libleela.so
