#!/bin/sh

. /etc/default/leela-warpgrep

test_endpoint () {
  nc -z $(echo "$1" | cut -d: -f1) $(echo "$1" | cut -d: -f2)
}

if test_endpoint "${LEELA_WARPGREP_ENDPOINT#tcp://}"
then
    if test_endpoint "${LEELA_WARPGREP_BUS_ENDPOINT#tcp://}"
    then exit 0
    else exit 2
    fi
else exit 2
fi
