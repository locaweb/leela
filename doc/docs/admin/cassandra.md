CASSANDRA
=========

Cassandra operation guide.

DISK USAGE
----------

If disk free space is running low:

1. `$ ssh linda0001`                     # any cassandra machine will do
2. `linda0001 $ cqlsh -k leela`          # provide the credentials as needed
3. `linda0001 $ cql> truncate t_attr_NN` # there are a table per month; pick one or more

These commands assume a health cluster. If that is not the case, use
`drop table` instead of truncate:

1. `$ ssh linda0001`
2. `linda0001 $ cqlsh -k leela`
3. `linda0001 $ cql> drop table t_attr_NN`         # now wait a suitable amount of time
4. `$ ssh warp0013`                                # any blackbox machine will do
5. `warp0013 $ /etc/init.d/leela-blackbox restart` # this will creates the missing table

### BACKUP ###

After these commands the data is gone forever. If you want to backup
it for whenever reason, before dropping or truncating the table:

1. `$ ssh linda0001`
2. `linda0001 $ nodetool snapshot -cf t_attr_NN leela` # you need to do this on *every machine*
3. `# backup the data: /var/lib/cassandra/data/leela/t_attr_NN-*/snapshots`
4. `linda0001 $ nodetool clearsnapshot`                # you need to do this on *every machine*

REPAIR
------

Repair is a regular process in cassandra that conciliates
information. You need to run it regularly. Currently it repair only
the graph database and runs once a day:

```
/etc/cron.d/cassandra-repair
```

It protects itself against simultaneous execution
[so there is no harm to run manually]. The lock is keep at
`/tmp/cassandra-repair`, in case the process get stuck.

N.B.: Do not ever delete this file while there is a repair running. It
is a cost operation and causes a considerable increase of load on the
cluster. Running multiple instances may cause severe performance
degradation.

DISK FAILURE/MACHINE FAILURE
----------------------------

In the event of a disk/machine failure, replaces the disk and starts
the cassandra with the following flag:

```
JVM_OPTS="$JVM_OPTS -Dcassandra.replace_address=IP_OF_THE_DEAD_NODE" /etc/init.d/cassandra start
```

If the IP is the same cassandra won't start without this flag. On the
other hand, **if the IP did change, you must not start cassandra
without this flag**. This is very important. Cassandra will bootstrap
a new node instead of replacing the old one. If you ever do this you
will have to decommission the dead node which will incur in much more
data moving around.
