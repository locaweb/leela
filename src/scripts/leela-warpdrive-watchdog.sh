#!/bin/sh

set -e

. /etc/default/leela-warpdrive

echo "$LEELA_WARPDRIVE_ENDPOINT" | cut -c 7- | { IFS=:; read host port &&
  nc -z $host $port && {
    echo -n "-- hostname: "; hostname
    echo "tcp://$host:$port;"
  }
}
