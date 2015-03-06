
srcroot    = $(CURDIR)/../..
distroot  ?= /tmp/leela/dist
buildroot ?= /tmp/leela/build

clean:
	rm -rf $(buildroot)

$(buildroot):
	@echo using buildroot=$(buildroot)
	mkdir -p $(buildroot)

$(distroot):
	@echo using distroot=$(distroot)
	mkdir -p $(distroot)

compile.libleela: $(buildroot)
	cd $(buildroot) && { \
	  cmake $(srcroot) -DCMAKE_INSTALL_PREFIX=$(distroot); \
	  make && make install; \
	}

compile.libleela-python: $(buildroot) $(distroot)
	cd "$(srcroot)/src/libs/python" && { \
	  for pybin in python2.5 python2.6 python2.7; \
	  do \
	    if command -v $$pybin >/dev/null 2>/dev/null; \
	    then \
	      env CFLAGS=-I$(distroot)/include \
	          LDFLAGS=-L$(distroot)/lib \
	          $$pybin setup.py build --build-base=$(buildroot) install --root=$(distroot); \
	      break; \
	    fi; \
	  done \
	}
