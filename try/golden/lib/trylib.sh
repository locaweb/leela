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
}

leela_trylib_dmproc_start () {
  rm -f $dbusfile
  rm -f $sockfile
  ./usr/bin/dmproc -vvv $dbusfile $sockfile&
  echo -n "$!" >$pidfile
  leela_trylib_wait_file $dbusfile
  leela_trylib_wait_file $sockfile
}

leela_trylib_service_stop () {
  test -f $pidfile && kill $(cat $pidfile)
  while test -f $pidfile
  do sleep 1; done
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

leela_trylib_cassandra_execute () {
  cassandra-cli -h localhost -k leela
}

leela_trylib_cassandra_drop () {
  cat <<EOF | cassandra-cli -B -h localhost
drop keyspace leela;
EOF
}

leela_trylib_cassandra_creat () {
  cat <<EOF | cassandra-cli -B -h localhost
create keyspace leela
  with placement_strategy = 'SimpleStrategy'
  and strategy_options = {replication_factor : 1}
  and durable_writes = true;

use leela;

create column family events
  with column_type = 'Standard'
  and comparator = 'ReversedType(org.apache.cassandra.db.marshal.Int32Type)'
  and default_validation_class = 'DoubleType'
  and key_validation_class = 'UTF8Type'
  and read_repair_chance = 1.0
  and dclocal_read_repair_chance = 0.0
  and gc_grace = 864000
  and min_compaction_threshold = 4
  and max_compaction_threshold = 32
  and replicate_on_write = true
  and compaction_strategy = 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy'
  and caching = 'KEYS_ONLY'
  and compression_options = {'chunk_length_kb' : '64', 'sstable_compression' : 'org.apache.cassandra.io.compress.DeflateCompressor'};

create column family data
  with column_type = 'Standard'
  and comparator = 'ReversedType(org.apache.cassandra.db.marshal.Int32Type)'
  and default_validation_class = 'UTF8Type'
  and key_validation_class = 'UTF8Type'
  and read_repair_chance = 1.0
  and dclocal_read_repair_chance = 0.0
  and gc_grace = 864000
  and min_compaction_threshold = 4
  and max_compaction_threshold = 32
  and replicate_on_write = true
  and compaction_strategy = 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy'
  and caching = 'KEYS_ONLY'
  and compression_options = {'chunk_length_kb' : '64', 'sstable_compression' : 'org.apache.cassandra.io.compress.DeflateCompressor'};
EOF
}
