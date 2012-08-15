#!/bin/sh

leela_root=${leela_root:-$(pwd)}
bin_sed=${bin_sed:-/bin/sed}

major=$1
minor=$2
build=$3

read_version() {
  [ -z "$major" ] && read -p "major: " major
  [ -z "$minor" ] && read -p "minor: " minor
  [ -z "$build" ] && read -p "build: " build
  version="$major.$minor.$build"
}

print_usage() {
  echo "[usage] version.sh MAJOR MINOR BUILD"
}

check_environ() {
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

update_version() {
  echo " updating file: $1"
  $bin_sed -i -r 's/\$version[^\$]*\$/\$version '"$version"'$/' "$1"
  $bin_sed -i -r 's/^(\s*)version(\s*)=(\s*)["'\''][0-9]+\.[0-9]+\.[0-9]+["'\'']/\1version\2=\3"'"$version"'"/' $1
}

write_pyversion() {
  echo " creating file: $1"
  cat <<EOF >"$1"
#!/usr/bin/python
# -*- coding: utf-8; -*-
#
# All Rights Reserved.
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

# DO NOT EDIT, AUTOMATICALLY GENERATED

major   = "$major"
minor   = "$minor"
build   = "$build"
version = "$major.$minor.$build"

EOF
}

read_version
check_environ
echo "version: $version"
update_version "$leela_root/README.rst"
update_version "$leela_root/setup.py"
write_pyversion "$leela_root/src/leela/server/version.py"

