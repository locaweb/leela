#!/usr/bin/env bash

basedir=$(dirname $0)

if [ -e /opt/zmq4/lib/pkgconfig ]; then
  export PKG_CONFIG_PATH=/opt/zmq4/lib/pkgconfig
fi

# BUILD WARPDRIVE
cd ${basedir}/leela/src/warpdrive; rm -r dist; cabal update; cabal sandbox init; \
  cabal install --only-dependencies --force-reinstalls; cabal configure -O2; cabal build
