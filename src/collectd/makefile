poly1305aes.srcroot  = $(CURDIR)/../../lib/poly1305aes

CFLAGS               = -Wall

build: poly1305aes.build compile

compile: override CPPFLAGS += -D_POSIX_C_SOURCE=200112 -D__USE_XOPEN -D_XOPEN_SOURCE
compile: override CFLAGS += -Isrc -std=c99 -pedantic -fPIC -pthread -I$(poly1305aes.h)

compile:
	$(CC) $(CPPFLAGS) $(CFLAGS) -shared -I../c/src write_leela.c ../c/src/leela/*.c $(poly1305aes.a) -owrite_leela.so -Wl,-soname,write_leela -lzmq -lm

clean: poly1305aes.clean
	rm -f write_leela.so

include $(poly1305aes.srcroot)/makefile
