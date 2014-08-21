#!/usr/bin/env bash

basedir=$(dirname $0)

main() {
  check_installation
  create_dirs
}

check_installation() {
  if [ ! hash redis-server 2>/dev/null ]; then
    apt-get install -y --allow-unauthenticated redis-server
    /etc/init.d/redis-server stop
  fi
}

create_dirs() {
  mkdir -p /var/lib/redis/
  mkdir -p /var/log/leela/redis
  mkdir -p /var/lib/redis/{master,slave}
}

main
