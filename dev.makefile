srcroot=$(CURDIR)

bin_find      = find
bin_ghc       = ghc
bin_rm        = rm
bin_nosetests = nosetests
nosetestsargs = 
ghcargs       = 
pyenv         = env PYTHONPATH=$(srcroot)/src/server
userfile     := $(shell test -f $$HOME/leela/user.mk && echo $$HOME/leela/user.mk)

ifneq ($(userfile),"")
include $(userfile)
endif

default:
	@echo "dev.makefile"
	@echo "============"
	@echo "Helps you performing various developets tasks."
	@echo
	@echo "  * clean         Removes all temporary files"
	@echo "  * compile       Compiles the dmproc code"
	@echo "  * test_dmproc   Run dmproc tests"
	@echo "  * test_server   Run server tests"
	@echo "  * test          Run all tests"

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
