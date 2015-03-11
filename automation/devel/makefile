
srcroot    = "$(CURDIR)/../.."
distroot  ?= /tmp/leela/dist
buildroot ?= /tmp/leela/build

bin_cabal ?= cabal

clean:
	rm -rf "$(buildroot)"

$(buildroot)/%:
	@echo using buildroot="$(buildroot)"
	mkdir -p "$@"

$(distroot):
	@echo using distroot="$(distroot)"
	mkdir -p "$(distroot)"

compile.libleela: $(buildroot)/libleela
	cd "$(buildroot)/libleela" && { \
	  cmake \
	    -DLEELA_BUILD_LIBLEELA=on \
	    -DCMAKE_INSTALL_PREFIX="$(distroot)" \
	    "$(srcroot)"; \
	  make && make install; \
	}

compile.libleela-python: $(buildroot)/python $(distroot) compile.libleela
	cd "$(srcroot)/src/libs/python" && { \
	  for pybin in python2.5 python2.6 python2.7; \
	  do \
	    if command -v $$pybin >/dev/null 2>/dev/null; \
	    then \
	      env CFLAGS=-I"$(distroot)/include" \
	          LDFLAGS=-L"$(distroot)/lib" \
	          $$pybin setup.py build --build-base="$(buildroot)" install --root="$(distroot)"; \
	      break; \
	    fi; \
	  done \
	}

compile.collectd: $(buildroot)/collectd $(distroot) compile.libleela
	cd "$(buildroot)/collectd" && { \
	  env CMAKE_INCLUDE_PATH="$(distroot)/include" \
	      CMAKE_LIBRARY_PATH="$(distroot)/lib" \
	    cmake \
	      -DLEELA_BUILD_COLLECTD=on \
	      -DCMAKE_INSTALL_PREFIX="$(distroot)" \
	      "$(srcroot)"; \
	  make && make install; \
	}

compile.warpdrive: $(buildroot)/warpdrive $(distroot) compile.libleela
	cd "$(buildroot)/warpdrive" && { \
	  test -d src || cp -a $(srcroot)/src/warpdrive src; \
	  cd src && { \
	    test -e cabal.sandbox.config || $(bin_cabal) sandbox init; \
	      $(bin_cabal) configure -O2 \
	        --extra-lib-dirs="$(distroot)/lib" \
	        --extra-include-dirs="$(distroot)/include" || { \
	      $(bin_cabal) install --only-dependencies -O2 --force-reinstalls; \
	      $(bin_cabal) configure -O2 \
	        --extra-lib-dirs="$(distroot)/lib" \
	        --extra-include-dirs="$(distroot)/include"; \
	    }; \
	    $(bin_cabal) build; \
	  }; \
	}
