#!/bin/sh

set -e

. /etc/default/leela/blackbox

echo "$LEELA_BLACKBOX_ENDPOINT" | cut -c 7- | { IFS=:; read host port &&
  nc -z $host $port && echo "tcp://$host:$port"
}
