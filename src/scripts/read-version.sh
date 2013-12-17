#!/bin/sh

leela_root=${leela_root:-$(pwd)}
bin_sed=${bin_sed:-/bin/sed}

curdir="$(cd $(dirname "$0") && pwd)"

major=$1
minor=$2
build=$3

read_version () {
  if [ -z "$1" -a -z "$2" -a -z "$3" ]
  then
    major=$(sed -r '/^v/ba; d; :a s/v([0-9]+)\.[0-9]+\.[0-9]+.*/\1/; q' "$curdir/../../CHANGELOG")
    minor=$(sed -r '/^v/ba; d; :a s/v[0-9]+\.([0-9]+)\.[0-9]+.*/\1/; q' "$curdir/../../CHANGELOG")
    build=$(sed -r '/^v/ba; d; :a s/v[0-9]+\.[0-9]+\.([0-9]+).*/\1/; q' "$curdir/../../CHANGELOG")
  else
    [ -z "$major" ] && read -p "major: " major
    [ -z "$minor" ] && read -p "minor: " minor
    [ -z "$build" ] && read -p "build: " build
  fi
  version="$major.$minor.$build"
}

check_environ () {
  test -z "$major" && {
    print_usage
    echo "major can not be blank" >&2
    exit 1
  }

  test -z "$minor" && {
    print_usage
    echo "minor can not be blank" >&2
    exit 1
  }

  test -z "$build" && {
    print_usage
    echo "build can not be blank" >&2
    exit 1
  }

  test -x "$bin_sed" || {
    echo "$bin_sed (sed) program not found or not executable" >&2
    exit 1
  }
}

if [ "$(basename $0)" = read-version.sh ]
then
  read_version
  echo $version
fi
