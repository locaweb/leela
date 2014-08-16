#!/usr/bin/env bash

basedir=$(dirname $0)

# UPDATE CABAL TO THE LATEST VERSION
cabal update; cabal install cabal cabal-install

# CREATE A LINK FOR THE UPDATED CABAL
mv /usr/bin/cabal /usr/bin/cabal.old; ln -s ~/.cabal/bin/cabal /usr/bin/cabal

# BUILD WARPDRIVE
cd ${basedir}/leela/src/warpdrive; cabal update; cabal sanbox init; \
  cabal install --only-dependencies --force-reinstalls; cabal configure -O2; cabal build
