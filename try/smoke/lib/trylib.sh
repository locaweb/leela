#!/bin/sh

set -x

srcroot=${srcroot:-$(pwd)}
pidfile=/tmp/try-leela.pid
logfile=/tmp/try-leela.log
dbusfile=/tmp/try-leela.dbus
sockfile=/tmp/try-leela.sock

http_endpoint=http://127.0.0.1:4080

bin_dmproc=${bin_dmproc:-./src/dmproc/DarkMatter/dmproc}
bin_timeline=${bin_timeline:-./src/dmproc/DarkMatter/timeline}
bin_twistd=${bin_twistd:-twistd}
bin_python=${bin_python:-python}
bin_socat=${bin_socat:-socat}
bin_lsof=${bin_lsof:-lsof}
bin_curl=${bin_curl:-curl}
bin_date=${bin_date:-date}
bin_sed=${bin_sed:-sed}

leela_trylib_xsock_read () {
  $bin_socat -t1 UNIX-RECVFROM:$1 STDOUT
}

leela_trylib_xsock_write () {
  $bin_socat -t1 STDIO UNIX-SENDTO:$1
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

leela_trylib_waitpid () {
  pid=$1
  for _ in 1 2 3 4 5
  do
    if ! ps -p $pid >/dev/null; then break; fi
  done
}

leela_trylib_wait_inet () {
  pidf=$1
  inet=$2
  while ! test -e $pidf && $bin_lsof -t -a -i$inet -p $(cat $pidf)
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
    --config=${srcroot}/try/smoke/cnf/leela.conf

  leela_trylib_wait_file $pidfile
}

leela_trylib_dmproc_stop () {
  test -f $pidfile && leela_trylib_stop $(cat $pidfile)
  rm -f $pidfile
}

leela_trylib_timeline_stop () {
  test -f $pidfile && leela_trylib_stop $(cat $pidfile)
  rm -f $pidfile
}

leela_trylib_service_stop () {
  test -f $pidfile && leela_trylib_stop $(cat $pidfile)
  leela_trylib_waitpidfile $pidfile
}

leela_trylib_stop () {
  pid=$1
  test -f $pidfile && kill $(cat $pidfile)
  leela_trylib_waitpid $pid
}

leela_trylib_dmproc_start () {
  rm -f $dbusfile
  rm -f $sockfile
  $bin_dmproc $dbusfile $sockfile &
  echo -n "$!" >$pidfile
  leela_trylib_wait_file $dbusfile
  leela_trylib_wait_file $sockfile
}

leela_trylib_timeline_start () {
  rm -f $dbusfile
  rm -f $sockfile
  $bin_timeline $dbusfile $sockfile&
  echo -n "$!" >$pidfile
  leela_trylib_wait_file $dbusfile
  leela_trylib_wait_file $sockfile
}

leela_trylib_udp_write () {
  $bin_socat -t2 STDIO UDP4:localhost:6968
}

leela_trylib_curl () {
  $bin_curl -s "$@"; echo
}

leela_trylib_xmpp_interact () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_python                        \
    ${srcroot}/try/smoke/lib/xmpp_interact.py "$@"
}

leela_trylib_dmproc_interact () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_python                        \
    ${srcroot}/try/smoke/lib/dmproc_interact.py --databus $dbusfile --socket $sockfile "$@"
}

leela_trylib_cassandra_interact () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_python                        \
    ${srcroot}/try/smoke/lib/cassandra_interact.py "$@"
}
