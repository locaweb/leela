#!/bin/sh

set -e

clojure_javarepo=${clojure_javarepo:-deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main}

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

clojure_ijava () {
  if ! has_file "/etc/apt/sources.list.d/java.list"
  then
    ubuntu_apt_key EEA14886
    deb_add_repo java "$clojure_javarepo"
    run_cmd_echo sh -c "echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections"
  fi
  deb_install oracle-java8-installer
}

clojure_ilein () {
  if ! has_command lein
  then
    fetch_url "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein" >"$distroot/bin/lein"
    run_cmd chmod 755 "$distroot/bin/lein"
  fi
}

show_self clojure_javarepo="\"$clojure_javarepo\""
clojure_ijava
run_installer "$distroot" "$buildroot" clojure_ilein
