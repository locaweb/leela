#!/bin/sh

set -e

. /etc/default/leela-redis

echo "$LEELA_REDIS_ENDPOINT" | cut -c 7- | { IFS=:; read host port &&
  redis-cli -h $host -p $port -a "$LEELA_REDIS_PASSWORD" info >/dev/null && {
    echo -n "-- uptime: "; uptime
    echo -n "-- hostname: "; hostname
    echo "tcp://$host:$port"
  }
}
