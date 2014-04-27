#!/bin/sh

. /etc/default/leela-redis

echo "$LEELA_REDIS_ENDPOINT" | cut -c 7- | { IFS=:; read host port &&
  redis-cli -h $host -p $port -a "$LEELA_REDIS_PASSWORD" info >/dev/null
}

if [ "$?" -eq 0 ]
then
  exit 0
else
  exit 2
fi
