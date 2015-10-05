CONSUL
======

Consul operation guide. Consul is used for service discovery. Leela
discover the machines by reading that information from consul.

UPGRADE
-------

First upgrade the servers, one by one. After that you may upgrade the
clients, again, one at a time.

CLUSTER INFO
------------

* `consul members`                              # returns the cluster members and its status
* `curl http://localhost:8500/v1/status/peers`  # current raft peers [usually three];
* `curl http://localhost:8500/v1/status/leader` # *must* return exactly one;

ADDING INSTANCES
----------------

1. make sure the directory `/etc/consul/conf.d` and the file
   `/etc/consul/config` are properly configure. That file should come
   from CFengine.
2. `/etc/init.d/consul start`

REMOVING INSTANCE
-----------------

Since the consul monitors a machine and all its services, if you ever
remove an instance you must also remove all of its services. Follow
the procedure to stop and remove every service individually and then
simply stops consul:

1. `/etc/init.d/consul stop`

CONFIG FILES
------------

* `/etc/consul/config`

SERVICE FILES
-------------

If you ever change any of these files, issue `consul reload` to apply
the changes.

* `/etc/consul/conf.d/*.service`

BOOTSTRAPING A NEW CLUSTER
--------------------------

All machines have died, somehow! Then you need to put the cluster back
on:

1. `env CONSUL_EXTRA_OPTS="-bootstrap" /etc/init.d/consul start`

Wait this instance to get back online. It should be the sole instance
thus the leader. Now you can put the rest online normally
[the dns should be working too].

2. `/etc/init.d/consul start`
