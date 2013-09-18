#!/bin/sh

PATH=$HOME/.cabal/bin:$PATH
BUILDROOT="$1"
DISTFILE="$BUILDROOT/$2"

log_stderr () {
  if [ -z "$1" ]
  then
    cat | >&2
  else
    echo "$@" >&2
  fi
}

build_warpdrive () {
  local dist
  dist=$1

  log_stderr compiling warpdrive ...

  cd "$BUILDROOT/src/haskell"
  cabal update
  cabal configure
  if ! cabal-dev install -O2
  then exit -1; fi
  cp -apv "$BUILDROOT/src/haskell/cabal-dev/bin/warpdrive" "$dist/bin/warpdrive" | log_stderr
}

build_blackbox () {
  local dist src
  dist=$1

  log_stderr building blackbox ...

  cd "$BUILDROOT/src/clojure"
  if ! lein uberjar
  then exit -1; fi
  cp -v "$BUILDROOT/src/clojure/"/blackbox-*-standalone.jar "$dist/lib/blackbox.jar" | log_stderr
  cp -apv /usr/lib/libjzmq*.so* "$dist/lib/" | log_stderr
}

build_pack () {
  echo creating tar file $DISTFILE | log_stderr
  TMPTAR=$(mktemp) && {
    cd "$BUILDROOT/dist"
    tar -c -z -f "$TMPTAR" .
    mv "$TMPTAR" "$DISTFILE"
    rm -f "$TMPTAR"
  }
}

mkdir -p "$BUILDROOT/dist/bin"
mkdir -p "$BUILDROOT/dist/lib"

build_warpdrive "$BUILDROOT/dist"
build_blackbox "$BUILDROOT/dist"
build_pack
