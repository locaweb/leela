
CFLAGS = -W -Wall -pedantic

SRC_FILES = $(wildcard src/leela/*.c)
OBJ_FILES = $(subst .c,.o,$(SRC_FILES))

$(OBJ_FILES): override CFLAGS += -Isrc -std=c99

compile: $(OBJ_FILES)

try_leela: override CPPFLAGS += -DLEELA_TRACING
try_leela: override CXXFLAGS += -g -Isrc -Itry -I/usr/include/unittest++
try_leela: override LDLIBS += -lUnitTest++ -lzookeeper_mt -lpthread -lzmq
try_leela: try/*.cc $(OBJ_FILES)

test: try_leela
	nc -z localhost 2181       || { echo "could not connect to zookeeper: localhost:2181"; exit 1; }
	[ -n "$(LEELA_ENDPOINT)" ] || { echo "missing environ variable: LEELA_ENDPOINT (eg.: LEELA_ENDPOINT='tcp://localhost:4080;'"; exit 1; }
	env LEELA_ENDPOINT="$(LEELA_ENDPOINT)" ./try_leela

clean:
	rm -f $(OBJ_FILES)
	rm -f try_leela
