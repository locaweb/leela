#!/bin/sh

ghc -W -Wall -fforce-recomp -threaded -isrc/core -O2 --make -static -optc-static -optl-static ./src/core/DarkMatter/dmproc.hs -optl-pthread && cp -p src/core/DarkMatter/dmproc usr/bin/dmproc
ghc          -fforce-recomp -threaded -isrc/core -itry/core -static -optc-static -optl-static -O2 --make ./try/core/Test/DarkMatter/dmtry.hs -optl-pthread && cp -p try/core/Test/DarkMatter/dmtry usr/bin/dmtry
