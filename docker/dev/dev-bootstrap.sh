#!/bin/sh

set -e

apt-get update && apt-get install -q --yes --force-yes \
  git socat gcc g++ make autotools-dev autoconf \
  automake pkg-config libtool zlib1g-dev libzmq3-dev \
  ghc cabal-install leiningen

cd /var/tmp && unzip jzmq-v2.2.2.zip
cd jzmq-2.2.2 && ./autogen.sh && ./configure --prefix=/usr && make && make install

cabal update && cabal install cabal-dev
