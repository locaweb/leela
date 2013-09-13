#!/bin/sh

PATH=$HOME/.cabal/bin:$PATH

log_stderr () {
  if [ -z "$1" ]
  then
    cat | >&4
  else
    echo "$@" >&4
  fi
}

recv () {
  log_stderr receiving package to build ...
  socat -u TCP-LISTEN:9999 EXEC:"tar -x -z -C \"$BUILDROOT\""
}

send () {
  log_stderr sending binary package ...
  cd "$BUILDROOT/dist"
  tar -c -z . >&3
}

build_warpdrive () {
  local dist
  dist=$1

  log_stderr compiling warpdrive ...

  cd "$BUILDROOT/src/haskell"
  cabal update
  cabal configure
  cabal-dev install -O2
  cp -v "$BUILDROOT/src/haskell/dist/build/warpdrive/warpdrive" "$dist/bin/warpdrive" | log_stderr
}

build_blackbox () {
  local dist src
  dist=$1

  log_stderr building blackbox ...

  cd "$BUILDROOT/src/clojure"
  lein uberjar
  cp -v "$BUILDROOT/src/clojure/target"/blackbox-*-standalone.jar "$dist/lib/blackbox.jar" | log_stderr
  cp -apv /usr/lib/libjzmq*.so* "$dist/lib/" | log_stderr
}

BUILDROOT=$(mktemp -d) && {
  buildlog=$BUILDROOT.buildlog
  exec 3>&1
  exec 4>&2
  exec >"$buildlog" 2>&1

  mkdir -p "$BUILDROOT/dist/bin"
  mkdir -p "$BUILDROOT/dist/lib"
  mkdir -p "$BUILDROOT/tmp/"

  recv
  build_warpdrive "$BUILDROOT/dist"
  build_blackbox "$BUILDROOT/dist"
  cp -v "$buildlog" "$BUILDROOT/tmp/buildlog" | log_stderr
  send

  rm -rf "$BUILDROOT"
}
