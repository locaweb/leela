CC := g++ -g
LIBS := -L/usr/lib/x86_64-linux-gnu/ -lzmq -L. -lUnitTest++
INC := -I/home/alex/Downloads/UnitTest++/src
CFLAGS := -fPIC -g -Wall -Werror $(INC)
LFLAGS := $(LIBS)
MAJOR := 0
MINOR := 0
BUILD := 1
NAME := lql
VERSION := $(MAJOR).$(MINOR).$(BUILD)

lib: lib$(NAME).so.$(VERSION)

splint: $(NAME)_test
		LD_LIBRARY_PATH=. splint -I/usr/include/x86_64-linux-gnu/ *.c

test: $(NAME)_test
		LD_LIBRARY_PATH=. ./$(NAME)_test

gdb: $(NAME)_test
		LD_LIBRARY_PATH=. gdb ./$(NAME)_test

valg: $(NAME)_test
		LD_LIBRARY_PATH=. valgrind --tool=memcheck --leak-check=yes --track-origins=yes ./$(NAME)_test

$(NAME)_test: lib$(NAME).so
		$(CC) $(NAME)_test.cpp -O0 -o $@ -L. -l$(NAME) $(INC) $(LIBS)

lib$(NAME).so: lib$(NAME).so.$(VERSION)
		ldconfig -v -n .
			ln -s lib$(NAME).so.$(MAJOR) lib$(NAME).so

lib$(NAME).so.$(VERSION): $(NAME).o
		$(CC) -shared -Wl,-soname,lib$(NAME).so.$(MAJOR) $^ -O0 -o $@ $(LIBS)

clean:
		$(RM) $(NAME)_test *.o *.so*
