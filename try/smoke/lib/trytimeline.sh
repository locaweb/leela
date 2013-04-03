#!/bin/sh

leela_trytimeline_interact () {
  local peerfile
  local IFS
  local input
  peerfile=$1

  leela_trylib_timeline_start >/dev/null

  rm -f $peerfile
  (leela_trylib_xsock_read $peerfile; echo) &
  leela_trylib_wait_file $peerfile
  echo -n $peerfile | leela_trylib_xsock_write $sockfile

  IFS=
  input=""
  while read line
  do input="$input$line";  done
  echo -n "$input" | leela_trylib_xsock_write $dbusfile

  leela_trylib_timeline_stop
}
