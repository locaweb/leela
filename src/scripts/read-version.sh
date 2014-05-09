#!/bin/sh

leela_root=${leela_root:-$(pwd)}
bin_sed=${bin_sed:-/bin/sed}

curdir="$(cd $(dirname "$0") && pwd)"

read_version () {
  f="CHANGELOG"
  if [ -z "$major"  ]
  then
    major=$(sed -r '/^v/ba; d; :a s/v([0-9]+)\.[0-9]+\.[0-9]+.*/\1/; q' "$curdir/../../${f}")
  fi
  if [ -z "$minor" ]
  then
    minor=$(sed -r '/^v/ba; d; :a s/v[0-9]+\.([0-9]+)\.[0-9]+.*/\1/; q' "$curdir/../../${f}")
  fi
  if [ -z "$patch" ]
  then
    patch=$(sed -r '/^v/ba; d; :a s/v[0-9]+\.[0-9]+\.([0-9]+).*/\1/; q' "$curdir/../../${f}")
    build=$patch
  fi

  version="$major.$minor.$patch"
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

  test -z "$patch" && {
    print_usage
    echo "patch can not be blank" >&2
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
