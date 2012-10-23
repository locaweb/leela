#!/bin/sh

ghc -W -Wall -fforce-recomp -threaded -isrc/core -O2 --make -static -optc-static -optl-static ./src/core/DarkMatter/dmproc.hs -optl-pthread
cp -p src/core/DarkMatter/dmproc usr/bin/dmproc
