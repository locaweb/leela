#!/bin/sh

set -e

javarepo=${javarepo:-deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main}

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

install_java () {
  if ! has_file "/etc/apt/sources.list.d/java.list"
  then
    ubuntu_apt_key EEA14886
    debian_add_repo java "$java_repo"
    run_cmd_echo sh -c "echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections"
  fi
  debian_apt_get oracle-java8-installer
}

install_lein () {
  if ! has_command lein
  then
    fetch_url "https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein" >"$distroot/bin/lein"
    run_cmd chmod 755 "$distroot/bin/lein"
  fi
}

show_self javarepo="\"$javarepo\""
install_java
run_installer "$distroot" "$buildroot" install_lein
