
srcroot  = $(CURDIR)
userfile = $(HOME)/.leela-server.makefile

include $(srcroot)/makefile.lib

bootstrap: .saverc
	test -d $(HOME)/pyenv/leela-server || $(bin_virtualenv) $(HOME)/pyenv/leela-server
	$(HOME)/pyenv/leela-server/bin/pip install -q -r $(srcroot)/pip-requires.txt
	$(HOME)/pyenv/leela-server/bin/pip install -q nose
	$(HOME)/pyenv/leela-server/bin/pip install -q mock
	$(bin_cabal) update
	$(bin_cabal) install -v0 -O2 attoparsec
	$(bin_cabal) install -v0 -O2 vector
	$(bin_cabal) install -v0 -O2 blaze-builder
	$(bin_cabal) install -v0 -O2 double-conversion
	$(bin_cabal) install -v0 -O2 regex-tdfa
	$(bin_cabal) install -v0 -O2 stm
	$(bin_cabal) install -v0 -O2 quickcheck
	$(bin_cabal) install -v0 -O2 hslogger
	$(bin_cabal) install -v0 -O2 hspec
	$(bin_cabal) install -v0 -O2 shelltestrunner

clean:
	$(call .check_bin,find)
	$(bin_find) . -type f -name \*.o -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.pyc -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	rm -f ./src/dmproc/DarkMatter/dmproc ./try/dmproc/dmtry

compile-dmtry:
	$(call .check_bin,ghc)
	$(call .check_bin,dash)
	env bin_dash=$(bin_dash) $(bin_ghc) $(ghcargs) -v0 -i$(srcroot)/src/dmproc -threaded -i$(srcroot)/try/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/try/dmproc/dmtry.hs -optl-pthread

compile-dmproc:
	$(call .check_bin,ghc)
	$(bin_ghc) $(ghcargs) -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/src/dmproc/DarkMatter/dmproc.hs -optl-pthread
	$(bin_ghc) $(ghcargs) -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/src/dmproc/DarkMatter/timeline.hs -optl-pthread

build-dmproc: compile-dmproc
	mkdir -p $(srcroot)/usr/bin
	$(bin_install) -m 0755 $(srcroot)/src/dmproc/DarkMatter/dmproc $(srcroot)/usr/bin/dmproc
	$(bin_install) -m 0755 $(srcroot)/src/dmproc/DarkMatter/dmproc $(srcroot)/usr/bin/timeline

test-dmproc: compile-dmtry
	$(srcroot)/try/dmproc/dmtry

test-server:
	$(call .check_bin,python)
	env $(pyenv) $(bin_nosetests) $(nosetestsargs) $(srcroot)/try/server

test: test-dmproc test-server

test-smoke: compile-dmproc
	$(call .check_bin,lsof)
	$(call .check_bin,python)
	$(call .check_bin,shelltest)
	$(call .check_bin,twistd)
	$(call .check_bin,socat)
	$(call .check_bin,curl)
	$(call .check_bin,date)
	$(call .check_bin,sed)
	@echo "test-smoke                         " >&2
	@echo "================================== " >&2
	@echo                                       >&2
	@echo "Testing with the followign env:    " >&2
	@echo "---------------------------------- " >&2
	@echo "  bin_twisted: $(bin_twistd)       " >&2
	@echo "   bin_python: $(bin_python)       " >&2
	@echo "    bin_socat: $(bin_socat)        " >&2
	@echo "     bin_lsof: $(bin_lsof)         " >&2
	@echo "bin_shelltest: $(bin_shelltest)    " >&2
	@echo "     bin_curl: $(bin_curl)         " >&2
	@echo "     bin_date: $(bin_date)         " >&2
	@echo "      bin_sed: $(bin_sed)          " >&2
	@echo                                       >&2
	@echo "Using the following leela.cfg:     " >&2
	@echo "---------------------------------- " >&2
	@cat $(srcroot)/try/smoke/cnf/leela.conf    >&2
	@echo                                       >&2
	cd $(srcroot); env bin_twistd=$(bin_twistd) \
                           bin_lsof=$(bin_lsof)     \
                           bin_python=$(bin_python) \
                           bin_socat=$(bin_socat)   \
                           bin_curl=$(bin_curl)     \
                           bin_sed=$(bin_sed)       \
                           bin_date=$(bin_date)     \
                           $(bin_shelltest) $(shelltestargs) -c $(shelltestpath) -- --timeout=10

%: %.test
	$(MAKE) $(MAKEARGS) test-smoke shelltestpath=$^

.SUFFIXES: .test
