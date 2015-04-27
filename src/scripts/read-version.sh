#!/bin/sh

leela_root=${leela_root:-$(pwd)}
bin_sed=${bin_sed:-/bin/sed}

curdir="$(cd $(dirname "$0") && pwd)"

run_sed () {
  if [ "$(echo . | sed -r -e 's/(.)/\1/' -e q 2>/dev/null)" = "." ]
  then $bin_sed -r "$@"; fi

  if [ "$(echo . | sed -E -e 's/(.)/\1/' -e q 2>/dev/null)" = "." ]
  then $bin_sed -E "$@"; fi
}

read_version () {
  f="CHANGELOG${component}"
  if [ -z "$major"  ]
  then
    major=$(run_sed -e '/^v/ba' -e d -e :a -e 's/v([0-9]+)\.[0-9]+\.[0-9]+.*/\1/' -e q "$curdir/../../${f}")
  fi
  if [ -z "$minor" ]
  then
    minor=$(run_sed -e '/^v/ba' -e d -e :a -e 's/v[0-9]+\.([0-9]+)\.[0-9]+.*/\1/' -e q "$curdir/../../${f}")
  fi
  if [ -z "$patch" ]
  then
    patch=$(run_sed -e '/^v/ba' -e d -e :a -e 's/v[0-9]+\.[0-9]+\.([0-9]+).*/\1/' -e q "$curdir/../../${f}")
  fi

  if [ -n "$format" ]
  then
    version=$(eval echo "$format")
  else
    version="$major.$minor.$patch"
  fi
  echo "reading file: $f : $version" >&2
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
