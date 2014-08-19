#!/usr/bin/env bash

basedir=$(dirname $0)

# BUILD WARPDRIVE
cd ${basedir}/leela/src/warpdrive; cabal update; cabal sandbox init; \
  cabal install --only-dependencies --force-reinstalls; cabal configure -O2; cabal build
