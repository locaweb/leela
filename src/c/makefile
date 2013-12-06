
CFLAGS = -W -Wall -std=c99 -pedantic

SRC_FILES = $(wildcard src/leela/*.c)
OBJ_FILES = $(subst .c,.o,$(SRC_FILES))

compile: override CFLAGS += -Isrc
compile: $(OBJ_FILES)

try_leela: try/*.cc $(OBJ_FILES)

test: override CFLAGS += -g -Isrc
test: override LDLIBS += -lUnitTest++
test: override CXXFLAGS += -g -Isrc -I/usr/include/unittest++
test: try_leela
	./try_leela

# test: override CXXFLAGS += -Isrc -I/usr/include/unittest++
# test: override LDFLAGS += -lUnitTest++
# test: compile
# 	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -otry_leela try/*.cc $(OBJ_FILES) $(LDFLAGS)

clean:
	rm -f $(OBJ_FILES)
	rm -f try_leela
