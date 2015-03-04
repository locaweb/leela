#!/bin/sh

set -e

haskell_ghcurl=${haskell_ghcurl:-https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.bz2}
haskell_cabalurl=${haskell_cabalurl:-https://www.haskell.org/cabal/release/cabal-install-1.22.0.0/cabal-install-1.22.0.0.tar.gz}

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

haskell_idd () {
  deb_install tar bzip2
  deb_install libgmp-dev zlib1g-dev
  deb_install make gcc g++
}

haskell_cabal_update () {
  run_cmd_once "cabal update" run_cmd_echo cabal update
}

haskell_icabal () {
  local cabaldir

  cabaldir="$buildroot/cabal-install-1.22.0.0"

  if ! has_command cabal
  then
    msg_info "INSTALL cabal"
    fetch_url "$haskell_cabalurl" | tar -x -z -C "$buildroot"
    cd "$cabaldir" && {
      run_cmd_echo ./bootstrap.sh --sandbox "$distroot/cabal"
      run_cmd_echo ln -s "$distroot/cabal/bin/cabal" "/usr/local/bin/cabal"
      haskell_cabal_update
    }
  fi

  if ! has_command happy
  then
    haskell_cabal_update
    rm -rf "$distroot/happy"
    mkdir -p "$distroot/happy"
    cd "$distroot/happy"
    run_cmd_echo cabal sandbox init
    run_cmd_echo cabal install happy --symlink-bindir "$distroot/bin"
  fi

  if ! has_command alex
  then
    haskell_cabal_update
    rm -rf "$distroot/alex"
    mkdir -p "$distroot/alex"
    cd "$distroot/alex"
    run_cmd_echo cabal sandbox init
    run_cmd_echo cabal install alex --symlink-bindir "$distroot/bin"
  fi
}

haskell_ighc () {
  local ghcdir

  ghcdir="$buildroot/ghc-7.8.4"

  if ! has_command ghc
  then
    msg_info "INSTALL ghc-$arch"
    fetch_url "$haskell_ghcurl" | tar -x -j -C "$buildroot"
    cd "$ghcdir" && {
      run_cmd_echo ./configure --prefix="$distroot"
      run_cmd_echo make install
    }
  fi
}

show_self haskell_ghcurl="\"$haskell_ghcurl\"" haskell_cabalurl="\"$haskell_cabalurl\""
if has_command dpkg apt-get
then haskell_idd; fi
check_command tar bzip2 wget make gcc g++
check_library libgmp libz

run_installer "$distroot" "$buildroot" haskell_ighc
run_installer "$distroot" "$buildroot" haskell_icabal
