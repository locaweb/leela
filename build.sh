#!/bin/sh

trap cleanup INT

cd $(dirname $(readlink -f "$0"))

sendfile () {
  while ! socat -u OPEN:"$1" TCP-CONNECT:127.0.0.1:9999 2>/dev/null
  do sleep 1; done
}

cleanup () {
  [ -f "$TARFILE" ] && rm -f "$TARFILE"
  exit 130
}

TARFILE=$(mktemp) && {
  tar -c . -f "$TARFILE"
  sendfile "$TARFILE" &
  sudo docker run -a stdout -p 9999:9999 leela:dev /bin/leela-recv-build >package.tgz
  rm -f "$TARFILE"
}
