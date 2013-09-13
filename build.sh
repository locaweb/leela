#!/bin/sh

trap cleanup INT

cd $(dirname $(readlink -f "$0"))

sudo=""
[ "$UID" -eq 0 ] || sudo="sudo"
docker="$sudo docker"

sendfile () {
  for _ in $(seq 1 60)
  do
    sleep 1
    if lsof -t -itcp:9999
    then break; fi
  done
  socat -u OPEN:"$1" TCP-CONNECT:127.0.0.1:9999 2>/dev/null
  echo "sending files to builder: ok"
}

build_image () {
  echo "starting builder ..."
  $docker run -a stdout -p 9999:9999 leela:dev /bin/leela-recv-build >docker/app/leela-dist.tgz
}

build_app () {
  echo "building app container ..."
  $sudo ./docker/app/make.sh
}

cleanup () {
  [ -f "$TARFILE" ] && rm -f "$TARFILE"
  exit 130
}

TARFILE=$(mktemp) && {
  tar -c -z . -f "$TARFILE"
  sendfile "$TARFILE" &
  build_image && build_app
  rm -f "$TARFILE"
}
