
srcroot        = $(CURDIR)

userfile       = $(HOME)/.leela-server.makefile

bin_which      = which

bin_find       = find
bin_ghc        = ghc
bin_virtualenv = virtualenv
bin_cabal      = cabal
bin_lsof       = lsof
bin_socat      = socat
bin_curl       = curl

bin_twistd     = twistd
bin_nosetests  = nosetests
bin_shelltest  = shelltest
bin_python     = python

pyenv          = PYTHONPATH=$(srcroot)/src/server
hsenv          = PATH=$$PATH:$(HOME)/.cabal/bin

nosetestsargs  =
ghcargs        =
shelltestargs  =

-include $(userfile)

default:
	@echo "dev.makefile                                   "
	@echo "============                                   "
	@echo "Helps you performing various development tasks "
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
	@echo "  * test-golden   Run all acceptance tests     "
	@echo

bootstrap:
	$(call check_bin,which)
	$(call check_bin,virtualenv)
	$(call check_bin,cabal)
	$(call check_bin,ghc)
	$(call check_bin,find)
	$(call check_bin,lsof)
	$(call check_bin,socat)
	$(call check_bin,curl)
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
	echo "bin_which      = $(bin_which)"                              >>$(userfile)
	echo "bin_curl       = $(bin_curl)"                               >>$(userfile)

	test -d $(HOME)/pyenv/leela-server || $(bin_virtualenv) $(HOME)/pyenv/leela-server
	$(HOME)/pyenv/leela-server/bin/pip install -q -r $(srcroot)/PYDEPS.txt
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
	$(call check_bin,find)
	$(bin_find) . -type f -name \*.o -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.pyc -exec rm -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec rm -f \{\} \;
	rm -f ./src/dmproc/DarkMatter/dmproc

compile-dmtry:
	$(call check_bin,ghc)
	$(call check_bin2,/bin/dash)
	env bin_dash=$(bin_dash) $(bin_ghc) $(ghcargs) -v0 -i$(srcroot)/src/dmproc -threaded -i$(srcroot)/try/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/try/dmproc/dmtry.hs -optl-pthread

compile-dmproc:
	$(call check_bin,ghc)
	$(bin_ghc) $(ghcargs) -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/src/dmproc/DarkMatter/dmproc.hs -optl-pthread

compile: compile-dmproc
	cp -p $(srcroot)/src/dmproc/DarkMatter/dmproc $(srcroot)/usr/bin/dmproc

test-dmproc: compile-dmtry
	$(srcroot)/try/dmproc/dmtry

test-server:
	$(call check_bin,python)
	env $(pyenv) $(bin_nosetests) $(nosetestsargs) $(srcroot)/try/server

test: test-dmproc test-server

test-golden:
	$(call check_bin,lsof)
	$(call check_bin,python)
	$(call check_bin,shelltest)
	$(call check_bin,twistd)
	$(call check_bin,socat)
	$(call check_bin,curl)
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
	@echo "     bin_curl: $(bin_curl)         " >&2
	@echo                                       >&2
	@echo "Using the following leela.cfg:     " >&2
	@echo "---------------------------------- " >&2
	@cat $(srcroot)/try/golden/cnf/leela.conf   >&2
	@echo                                       >&2
	cd $(srcroot); env bin_twistd=$(bin_twistd) \
                           bin_lsof=$(bin_lsof)     \
                           bin_python=$(bin_python) \
                           bin_socat=$(bin_socat)   \
                           bin_curl=$(bin_curl)     \
                           $(bin_shelltest) $(shelltestargs) -c $(srcroot)/try/golden -- --timeout=10

check_bin      = @(test -x $(bin_$(1)) || $(bin_which) $(bin_$(1)) >/dev/null) || {          \
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

check_bin2     = @(test -x $(1) || $(bin_which) $(1)) >/dev/null || {                        \
                        echo "$(1) not found!!!";                                            \
                        echo;                                                                \
                        exit 1; }
