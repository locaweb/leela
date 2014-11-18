ifndef poly1305aes.srcroot
poly1305aes.srcroot=$(CURDIR)
endif

poly1305aes.impl = $(shell cat "$(poly1305aes.srcroot)/poly1305aes-20050218/poly1305aes.impl")

poly1305aes.tar := $(poly1305aes.srcroot)/poly1305aes-20050218.tar.gz
poly1305aes.a   := $(poly1305aes.srcroot)/poly1305aes-20050218/poly1305aes.a
poly1305aes.h   := $(poly1305aes.srcroot)/dist

poly1305aes.clean:
	rm -rf $(poly1305aes.srcroot)/dist
	rm -rf $(poly1305aes.srcroot)/poly1305aes-20050218

poly1305aes.build: $(poly1305aes.a)

$(poly1305aes.a):
	mkdir -p $(poly1305aes.h)/poly1305aes
	tar -C $(poly1305aes.srcroot) -x -z -f $(poly1305aes.tar)
	$(MAKE) CC="$(CC) -fPIC" -C $(poly1305aes.srcroot)/poly1305aes-20050218
	cp $(poly1305aes.srcroot)/poly1305aes-20050218/poly1305aes.a $(poly1305aes.srcroot)/poly1305aes-20050218/libpoly1305aes.a
	cd $(poly1305aes.srcroot)/poly1305aes-20050218 && cp `cat FILES.lib` poly1305aes.h $(poly1305aes.h)/poly1305aes
