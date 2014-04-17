CFLAGS = -Wall

compile: override CPPFLAGS += -D_POSIX_SOURCE=200112 -D__USE_XOPEN -D_XOPEN_SOURCE
compile: override CFLAGS += -Isrc -std=c99 -pedantic -fPIC -pthread

compile:
	$(CC) $(CPPFLAGS) $(CFLAGS) -shared -I../c/src write_leela.c ../c/src/leela/*.c -owrite_leela.so -Wl,-soname,write_leela -lzmq

clean:
	rm -f write_leela.so
