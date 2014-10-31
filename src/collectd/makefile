poly1305aes.srcroot  = $(CURDIR)/../../lib/poly1305aes

CFLAGS               = -Wall

build: poly1305aes.build compile

compile: override CPPFLAGS += -D_POSIX_C_SOURCE=199309L -D_XOPEN_SOURCE -D_BSD_SOURCE -DHAS_SYM_SLEEP -DPOLY_USE_$(poly1305aes.impl)
compile: override CFLAGS += -Isrc -std=c99 -pedantic -fPIC -pthread -I$(poly1305aes.h)

compile:
	$(CC) $(CPPFLAGS) $(CFLAGS) -shared -I../libs/poly1305aes/src -I../libs/c/src write_leela.c ../libs/c/src/leela/*.c $(poly1305aes.a) -owrite_leela.so -Wl,-soname,write_leela -lzmq -lm

clean: poly1305aes.clean
	rm -f write_leela.so

include $(poly1305aes.srcroot)/makefile
