#!/bin/sh

srcroot=${srcroot:-$(pwd)}
pidfile=${pidfile:-/tmp/try-leela.pid}

bin_twistd=${bin_twistd:-$HOME/pyenv/leela-server/bin/twistd}

leela_trylib_setup () {
  rm -f /tmp/try-leela.pipe
  touch /tmp/try-leela.pipe
}

leela_trylib_teardown () {
  echo -n
}

leela_trylib_start () {
  srv=$1
  env PYTHONPATH=${srcroot}/src/server \
    $bin_twistd                        \
    --logfile=/tmp/try-leela.log       \
    --pidfile=$pidfile                 \
    leela                              \
    --service=$1                       \
    --config=${srcroot}/try/golden/leela.conf
  sleep 1
}

leela_trylib_stop () {
  test -f $pidfile && kill $(cat $pidfile)
}

leela_trylib_read () {
  cat /tmp/try-leela.pipe
}

leela_trylib_udp_sendto () {
  nc -q2 -u localhost 6968
}
