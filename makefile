
root     = $(srcroot)/dist
srcroot  = $(CURDIR)
userfile = $(HOME)/.leelarc

include $(srcroot)/makefile.lib

SRC_HASKELL = $(shell $(bin_find) $(srcroot)/src/dmproc -type f -name \*.hs)
TRY_HASKELL = $(shell $(bin_find) $(srcroot)/try/dmproc -type f -name \*.hs)

bootstrap: .saverc
	$(call check_bin,virtualenv)
	$(call check_bin,cabal)
	test -d $(HOME)/pyenv/leela || $(bin_virtualenv) $(HOME)/pyenv/leela
	$(HOME)/pyenv/leela/bin/pip install -q -r $(srcroot)/pip-requires.txt
	$(HOME)/pyenv/leela/bin/pip install -q nose
	$(HOME)/pyenv/leela/bin/pip install -q mock
	test -d $(HOME)/.cabal || $(bin_cabal) update
	$(bin_cabal) install -v0 -O2 double-conversion
	$(bin_cabal) install -v0 -O2 hprotoc
	$(bin_cabal) install -v0 -O2 shelltestrunner
	$(bin_cabal) install -v0 -O2 blaze-builder
	$(bin_cabal) install -v0 -O2 regex-tdfa
	$(bin_cabal) install -v0 -O2 quickcheck
	$(bin_cabal) install -v0 -O2 attoparsec
	$(bin_cabal) install -v0 -O2 hashable
	$(bin_cabal) install -v0 -O2 hslogger
	$(bin_cabal) install -v0 -O2 vector
	$(bin_cabal) install -v0 -O2 hspec
	$(bin_cabal) install -v0 -O2 stm

clean:
	$(call .check_bin,find)
	$(bin_find) . -type f -name \*.o -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.pyc -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	rm -rf ./build
	rm -f ./src/dmproc/DarkMatter/dmproc    \
              ./src/dmproc/DarkMatter/multicast \
              ./src/dmproc/DarkMatter/timeline  \
              ./try/dmproc/dmtry

compile-dmtry: $(srcroot)/try/dmproc/dmtry

compile-dmproc: $(srcroot)/src/dmproc/DarkMatter/dmproc $(srcroot)/src/dmproc/DarkMatter/timeline $(srcroot)/src/dmproc/DarkMatter/multicast

test-dmproc: compile-dmtry
	$(srcroot)/try/dmproc/dmtry

test-server:
	$(call .check_bin,python)
	env $(pyenv) $(bin_nosetests) $(nosetestsargs) $(srcroot)/try/server

test: test-dmproc test-server

test-functional:
	$(call .check_bin,lsof)
	$(call .check_bin,python)
	$(call .check_bin,shelltest)
	$(call .check_bin,twistd)
	$(call .check_bin,socat)
	$(call .check_bin,curl)
	$(call .check_bin,date)
	$(call .check_bin,sed)
	$(srcroot)/dist/etc/init.d/leela stop >/dev/null
	$(srcroot)/dist/etc/init.d/leela start >/dev/null &&                     \
          cd $(srcroot);                                                         \
          $(bin_shelltest) $(shelltestargs) -c $(shelltestpath) -- --timeout=60; \
          $(srcroot)/dist/etc/init.d/leela stop >/dev/null

dist-build: compile-dmproc

dist-clean:
	if [ -n "$(root)" -a "$(root)" != "/" ]; then rm -r -f "$(root)"; fi

dist-install:
	env bin_virtualenv=$(bin_virtualenv) \
            bin_python=$(bin_python)         \
            bin_pip=$(bin_pip)               \
            $(srcroot)/src/scripts/install.sh "$(root)" virtualenv "$(srcroot)"

$(srcroot)/src/dmproc/DarkMatter/dmproc: $(SRC_HASKELL)
	$(call .check_bin,ghc)
	$(bin_ghc) $(ghcargs) -rtsopts -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $@.hs -optl-pthread

$(srcroot)/src/dmproc/DarkMatter/timeline: $(SRC_HASKELL)
	$(call .check_bin,ghc)
	$(bin_ghc) $(ghcargs) -rtsopts -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $@.hs -optl-pthread

$(srcroot)/src/dmproc/DarkMatter/multicast: $(SRC_HASKELL)
	$(call .check_bin,ghc)
	$(bin_ghc) $(ghcargs) -rtsopts -v0 -W -Wall -fforce-recomp -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $@.hs -optl-pthread

$(srcroot)/try/dmproc/dmtry: $(SRC_HASKELL) $(TRY_HASKELL)
	$(call .check_bin,ghc)
	$(call .check_bin,dash)
	env bin_dash=$(bin_dash) $(bin_ghc) $(ghcargs) -rtsopts -v0 -i$(srcroot)/src/dmproc -i$(srcroot)/try/dmproc -O2 --make $@.hs
