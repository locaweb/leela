srcroot=$(CURDIR)

userfile      := $(shell test -f $$HOME/.leela-server/dev.mk && echo $$HOME/.leela-server/dev.mk)

bin_find       = find
bin_ghc        = ghc
bin_rm         = rm
bin_virtualenv = virtualenv
bin_cabal      = cabal
bin_nosetests  = nosetests
pyenv          = env PYTHONPATH=$(srcroot)/src/server

nosetestsargs  = 
ghcargs        = 

ifneq ($(userfile),"")
include $(userfile)
endif

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
	@echo "  * test_dmproc   Run dmproc tests             "
	@echo
	@echo "  * test_server   Run server tests             "
	@echo
	@echo "  * test          Run all tests                "
	@echo "==============================================="

bootstrap:
	test -d $(HOME)/.leela-server || mkdir $(HOME)/.leela-server
	echo "bin_nosetests  = $(HOME)/pyenv/leela-server/bin/nosetests" >$(HOME)/.leela-server/dev.mk
	echo "bin_virtualenv = $(bin_virtualenv)"                       >>$(HOME)/.leela-server/dev.mk
	echo "bin_cabal      = $(bin_cabal)"                            >>$(HOME)/.leela-server/dev.mk

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
	$(bin_find) . -type f -name \*.o -exec $(bin_rm) -f \{\} \;
	$(bin_find) . -type f -name \*.hi -exec $(bin_rm) -f \{\} \;
	$(bin_find) . -type f -name \*.pyc -exec $(bin_rm) -f \{\} \;

compile_dmtry:
	$(bin_ghc) $(ghcargs) -v0 -i$(srcroot)/src/dmproc -threaded -i$(srcroot)/try/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/try/dmproc/Test/DarkMatter/dmtry.hs -optl-pthread

compile_dmproc:
	$(bin_ghc) $(ghcargs) -v0 -W -Wall -fforce-recomp -threaded -i$(srcroot)/src/dmproc -O2 --make -static -optc-static -optl-static $(srcroot)/src/dmproc/DarkMatter/dmproc.hs -optl-pthread

compile: compile_dmproc compile_dmtry
	cp -p $(srcroot)/src/dmproc/DarkMatter/dmproc $(srcroot)/usr/bin/dmproc
	cp -p $(srcroot)/try/dmproc/Test/DarkMatter/dmtry $(srcroot)/usr/bin/dmtry

test_dmproc: compile_dmtry
	$(srcroot)/try/dmproc/Test/DarkMatter/dmtry

test_server:
	$(pyenv) $(bin_nosetests) $(nosetestsargs) $(srcroot)/try/server

test: test_dmproc test_server

golden-test:
	cd $(srcroot); $(HOME)/.cabal/bin/shelltest -c -p $(srcroot)/try/golden -- --timeout=60
