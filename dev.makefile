srcroot=$(CURDIR)

userfile       = $(HOME)/.leela-dev.makefile

bin_find       = find
bin_ghc        = ghc
bin_virtualenv = virtualenv
bin_cabal      = cabal
bin_lsof       = lsof
bin_socat      = socat

bin_twistd     = twistd
bin_nosetests  = nosetests
bin_shelltest  = shelltest
bin_python     = python

pyenv          = PYTHONPATH=$(srcroot)/src/server
hsenv          = PATH=$$PATH:$(HOME)/.cabal/bin

nosetestsargs  =
ghcargs        =
shelltestargs  =

check_bin      = @$(bin_$(1)) $(2) >/dev/null 2>/dev/null || {                               \
                        echo "bin_$(1) not found!!!";                                        \
                        echo;                                                                \
                        echo "Use bin_$(1) variable to fix this, as follows: ";              \
                        echo "  $$ $(MAKE) ... bin_$(1)=/path/to/file";                      \
                        echo;                                                                \
                        echo "Additionally, you also change the file:";                      \
                        echo "  $(userfile)";                                                \
                        echo;                                                                \
                        echo "so that it gets remembered next time";                         \
                        exit 1; }

check_bin2     = @$(1) $(2) >/dev/null 2>/dev/null || {                                      \
                        echo "$(1) not found!!!";                                            \
                        echo;                                                                \
                        exit 1; }


-include $(userfile)

default:
	@echo "dev.makefile                                   "
	@echo "============                                   "
	@echo "Helps you performing various developets tasks. "
	@echo
	@echo "  * bootstrap     Bootstrap the dev environment"
	@echo
	@echo "  * clean         Removes all temporary files  "
	@echo
	@echo "  * compile       Compiles the dmproc code     "
	@echo
	@echo "  * test-dmproc   Run dmproc tests             "
	@echo
	@echo "  * test-server   Run server tests             "
	@echo
	@echo "  * test          Run all tests                "
	@echo
	@echo "  * golden        Run all function tests       "
	@echo

bootstrap:
	$(call check_bin,virtualenv,--version)
	$(call check_bin,cabal,--version)
	$(call check_bin,ghc,--version)
	$(call check_bin,find,--version)
	$(call check_bin,lsof,-v)
	$(call check_bin,socat,-V)
	echo "bin_nosetests  = $(HOME)/pyenv/leela-server/bin/nosetests"   >$(userfile)
	echo "bin_virtualenv = $(bin_virtualenv)"                         >>$(userfile)
	echo "bin_twistd     = $(HOME)/pyenv/leela-server/bin/twistd"     >>$(userfile)
	echo "bin_python     = $(HOME)/pyenv/leela-server/bin/python"     >>$(userfile)
	echo "bin_ghc        = $(bin_ghc)"                                >>$(userfile)
	echo "bin_cabal      = $(bin_cabal)"                              >>$(userfile)
	echo "bin_shelltest  = $(HOME)/.cabal/bin/shelltest"              >>$(userfile)
	echo "bin_lsof       = $(bin_lsof)"                               >>$(userfile)
	echo "bin_find       = $(bin_find)"                               >>$(userfile)
	echo "bin_socat      = $(bin_socat)"                              >>$(userfile)

	test -d $(HOME)/pyenv/leela-server || $(bin_virtualenv) $(HOME)/pyenv/leela-server
	$(HOME)/pyenv/leela-server/bin/pip install -q argparse
	$(HOME)/pyenv/leela-server/bin/pip install -q twisted
	$(HOME)/pyenv/leela-server/bin/pip install -q wokkel
	$(HOME)/pyenv/leela-server/bin/pip install -q thrift
	$(HOME)/pyenv/leela-server/bin/pip install -q https://github.com/driftx/Telephus/tarball/releases/1.0.0_beta1
	$(HOME)/pyenv/leela-server/bin/pip install -q txredisapi
	$(HOME)/pyenv/leela-server/bin/pip install -q nose
	$(HOME)/pyenv/leela-server/bin/pip install -q mock

	test -d $(HOME)/.cabal || $(bin_cabal) update
	$(bin_cabal) install -v0 -O2 attoparsec
	$(bin_cabal) install -v0 -O2 blaze-builder
	$(bin_cabal) install -v0 -O2 double-conversion
	$(bin_cabal) install -v0 -O2 regex-tdfa
	$(bin_cabal) install -v0 -O2 stm
	$(bin_cabal) install -v0 -O2 quickcheck
	$(bin_cabal) install -v0 -O2 hslogger
	$(bin_cabal) install -v0 -O2 hspec
	$(bin_cabal) install -v0 -O2 shelltestrunner

clean:
	$(call check_bin,find,--version)
	$(bin_find) . -type f -name \*.o -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.pyc -exec rm -f \{\} \;

compile-dmtry:
	$(call check_bin,ghc,--version)
	$(call check_bin2,/bin/dash,-c "exit 0")
	env bin_dash=$(bin_dash) $(bin_ghc) $(ghcargs) -v0 -i$(srcroot)/src/dmproc -threaded -i$(srcroot)/try/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/try/dmproc/dmtry.hs -optl-pthread

compile-dmproc:
	$(call check_bin,ghc,--version)
	$(bin_ghc) $(ghcargs) -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/src/dmproc/DarkMatter/dmproc.hs -optl-pthread

compile: compile_dmproc
	cp -p $(srcroot)/src/dmproc/DarkMatter/dmproc $(srcroot)/usr/bin/dmproc

test-dmproc: compile_dmtry
	$(srcroot)/try/dmproc/dmtry

test-server:
	$(call check_bin,python,-V)
	env $(pyenv) $(bin_nosetests) $(nosetestsargs) $(srcroot)/try/server

test: test-dmproc test-server

test-golden:
	$(call check_bin,lsof,-v)
	$(call check_bin,python,-V)
	$(call check_bin,shelltest,--version)
	$(call check_bin,twistd,--version)
	$(call check_bin,socat,-V)
	@echo "Acceptance testing                 " >&2
	@echo "==================                 " >&2
	@echo                                       >&2
	@echo "Testing with the followign env:    " >&2
	@echo "---------------------------------- " >&2
	@echo "  bin_twisted: $(bin_twistd)       " >&2
	@echo "   bin_python: $(bin_python)       " >&2
	@echo "    bin_socat: $(bin_socat)        " >&2
	@echo "     bin_lsof: $(bin_lsof)         " >&2
	@echo "bin_shelltest: $(bin_shelltest)    " >&2
	@echo                                       >&2
	@echo "Using the following leela.cfg:     " >&2
	@echo "---------------------------------- " >&2
	@cat $(srcroot)/try/golden/cnf/leela.conf   >&2
	@echo                                       >&2
	cd $(srcroot); env bin_twistd=$(bin_twistd) \
                           bin_lsof=$(bin_lsof)     \
                           bin_python=$(bin_python) \
                           bin_socat=$(bin_socat)   \
                           $(bin_shelltest) $(shelltestargs) -c $(srcroot)/try/golden -- --timeout=10
