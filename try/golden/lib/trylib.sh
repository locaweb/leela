#!/bin/sh

srcroot=${srcroot:-$(pwd)}
pidfile=${pidfile:-/tmp/try-leela.pid}

bin_twistd=${bin_twistd:-$HOME/pyenv/leela-server/bin/twistd}

leela_trylib_fifo_creat () {
  mkfifo /tmp/try-leela.pipe
}

leela_trylib_fifo_drop () {
  rm -f /tmp/try-leela.pipe
}

leela_trylib_fifo_read () {
  dd status=noxfer iflag=nonblock if=/tmp/try-leela.pipe bs=4096 count=1 2>/dev/null
}

leela_trylib_fifo_write () {
  dd status=noxfer oflag=nonblock of=/tmp/try-leela.pipe bs=4096 count=1 2>/dev/null
}

leela_trylib_service_start () {
  srv=$1
  env PYTHONPATH=${srcroot}/src/server \
    $bin_twistd                        \
    --logfile=/dev/null                \
    --pidfile=$pidfile                 \
    leela                              \
    --service=$1                       \
    --log-level=debug                  \
    --config=${srcroot}/try/golden/cnf/leela.conf

  if [ $srv = "udp" ]
  then
    while ! lsof -t -a -i4 -iudp:6968 -p $(cat $pidfile)
    do sleep 0.1; done
  fi

  if [ $srv = "storage" ]
  then
    while ! lsof -t -a -p $(cat $pidfile) /tmp/try-leela.pipe
    do sleep 0.1; done
  fi

  if [ $srv = "xmpp" ]
  then
    while ! lsof -t -a -i4 -itcp:5222 -p $(cat $pidfile)
    do sleep 0.1; done
  fi
}

leela_trylib_service_stop () {
  test -f $pidfile && kill $(cat $pidfile)
  while [ -f $pidfile ]
  do sleep 0.1; done
}

leela_trylib_udp_write () {
  nc -q1 -u localhost 6968
}

leela_trylib_xmpp_singleshot () {
  env PYTHONPATH=${srcroot}/src/server \
    $bin_twistd                        \
    --logfile=/dev/null                \
    -n -o -y                           \
    ${srcroot}/try/golden/lib/xmpp_singleshot.py
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
EOF
}
