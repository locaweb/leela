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
    --logfile=/tmp/try-leela.log       \
    --pidfile=$pidfile                 \
    leela                              \
    --service=$1                       \
    --config=${srcroot}/try/golden/cnf/leela.conf
  sleep 1
}

leela_trylib_service_stop () {
  test -f $pidfile && kill $(cat $pidfile)
  sleep 1
}

leela_trylib_udp_write () {
  nc -q1 -u localhost 6968
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
