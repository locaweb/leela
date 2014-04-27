#!/bin/sh

. /etc/default/leela-blackbox

echo "$LEELA_BLACKBOX_ENDPOINT" | cut -c 7- | { IFS=:; read host port &&
  nc -z $host $port
}

if [ "$?" -eq 0 ]
then
  exit 0
else
  exit 2
fi
