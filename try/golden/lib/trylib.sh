#!/bin/sh

srcroot=${srcroot:-$(pwd)}
pidfile=${pidfile:-/tmp/try-leela.pid}
logfile=${logfile:-/tmp/try-leela.log}
dbusfile=${dbusfile:-/tmp/try-leela.dbus}
sockfile=${sockfile:-/tmp/try-leela.sock}

bin_twistd=${bin_twistd:-$HOME/pyenv/leela-server/bin/twistd}
bin_python=${bin_python:-$HOME/pyenv/leela-server/bin/python}
bin_lsof=${bin_lsof:-lsof}

leela_trylib_xsock_read () {
  socat UNIX-RECVFROM:$1 STDOUT
}

leela_trylib_xsock_write () {
  socat STDIN UNIX-SENDTO:$1
}

leela_trylib_wait_xsock () {
  pidf=$1
  sock=$2
  while ! $bin_lsof -t -U -a -p $(cat $pidf) $sock
  do sleep 1; done
}

leela_trylib_waitpidfile () {
  for _ in 1 2 3 4 5
  do
    if ! test -f $pidfile; then break; fi
  done
  rm -f $pidfile
}

leela_trylib_wait_inet () {
  pidf=$1
  inet=$2
  while ! $bin_lsof -t -a -i$inet -p $(cat $pidf)
  do sleep 1; done
}

leela_trylib_wait_file () {
  file=$1
  while ! test -e $file
  do sleep 1; done
}

leela_trylib_service_start () {
  test -f $pidfile && leela_trylib_service_stop
  srv=$1
  env PYTHONPATH=${srcroot}/src/server \
    $bin_twistd                        \
    --logfile=$logfile                 \
    --pidfile=$pidfile                 \
    leela                              \
    --service=$1                       \
    --log-level=debug                  \
    --config=${srcroot}/try/golden/cnf/leela.conf

  leela_trylib_wait_file $pidfile
}

leela_trylib_dmproc_stop () {
  test -f $pidfile && kill $(cat $pidfile)
  sleep 1; rm -f $pidfile
}

leela_trylib_dmproc_start () {
  rm -f $dbusfile
  rm -f $sockfile
  ./usr/bin/dmproc $dbusfile $sockfile &
  echo -n "$!" >$pidfile
  leela_trylib_wait_file $dbusfile
  leela_trylib_wait_file $sockfile
}

leela_trylib_service_stop () {
  test -f $pidfile && kill $(cat $pidfile)
  leela_trylib_waitpidfile $pidfile
}

leela_trylib_udp_write () {
  socat -t1  STDIN UDP4-SENDTO:localhost:6968
}

leela_trylib_xmpp_interact () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_python                        \
    ${srcroot}/try/golden/lib/xmpp_interact.py "$@"
}

leela_trylib_dmproc_interact () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_python                        \
    ${srcroot}/try/golden/lib/dmproc_interact.py --databus $dbusfile --socket $sockfile "$@"
}

leela_trylib_cassandra_interact () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_python                        \
    ${srcroot}/try/golden/lib/cassandra_interact.py "$@"
}
