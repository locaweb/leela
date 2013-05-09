#!/bin/sh

now=$(date +%s)
rnd=$(( RANDOM ))
key_udp=leela.selftest.${now}_${rnd}.udp
key_http=leela.selftest.${now}_${rnd}.http
len_udp=$(echo -n $key_udp | wc -c)
len_http=$(echo -n $key_http | wc -c)

leela_interact () {
  $CHDIR/usr/libexec/leela-interact --config=$CHDIR/etc/leela.conf
}

test_udp_ping () {
  echo pong >$1/exp

  cat <<EOF | leela_interact >$1/out
udp-send message=ping\n
EOF
}

test_udp_write () {
  cat <<EOF >$1/exp
{"results": {"$key_udp": {"series": [[0, ${rnd}.0]]}}, "status": 200}
event $len_udp|$key_udp ${rnd}.0 0.0;
EOF

  cat <<EOF | leela_interact >$1/out
dmproc-connect "proc=proc match 15|^leela.selftest id;"
udp-send "message=gauge $len_udp|$key_udp $rnd {now};"
http-request method=GET url=/v1/past24/$key_udp
dmproc-disconnect
EOF
}

test_http_read_after_udp () {
  cat <<EOF >$1/exp
{"results": {"$key_udp": {"series": [[0, ${rnd}.0]]}}, "status": 200}
EOF

  cat <<EOF | leela_interact >$1/out
http-request method=GET url=/v1/past24/$key_udp
EOF
}

test_http_write () {
  cat <<EOF >$1/exp
{"results": [{"name": "$key_http", "timestamp": 0.0, "type": "gauge", "value": ${rnd}.0}], "status": 201}
event $len_http|$key_http ${rnd}.0 0.0;
EOF

  cat <<EOF | leela_interact >$1/out
dmproc-connect "proc=proc match 15|^leela.selftest id;"
http-request method=POST url=/v1/$key_http 'data={"type": "gauge", "value": ${rnd}, "timestamp": {now}}'
dmproc-disconnect
EOF
}

test_http_read_after_http () {
  cat <<EOF >$1/exp
{"results": {"$key_http": {"series": [[0, ${rnd}.0]]}}, "status": 200}
EOF

  cat <<EOF | leela_interact >$1/out
http-request method=GET url=/v1/past24/$key_http
EOF
}

for t in test_udp_ping   \
         test_udp_write  \
         test_http_read_after_udp   \
         test_http_write \
         test_http_read_after_http
do
  TEMPFILE=$(mktemp -d) && {
    $t $TEMPFILE
    if cmp $TEMPFILE/exp $TEMPFILE/out
    then
      echo "$t: ok"
      rm -rf $TEMPFILE
    else
      diff $TEMPFILE/out $TEMPFILE/exp
      rm -rf $TEMPFILE
      exit 1
    fi
  }
done
