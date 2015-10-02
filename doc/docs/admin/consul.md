CONSUL
======

Consul operation guide.

UPGRADE
-------

First upgrade the servers, one by one. After that you may upgrade the
clients, again, one at a time.

CLUSTER INFO
------------

* `consul members`                              # returns the cluster members and its status
* `curl http://localhost:8500/v1/status/peers`  # current raft peers [usually three];
* `curl http://localhost:8500/v1/status/leader` # *must* return exactly one;

CONFIG FILES
------------

* `/etc/consul/config`

SERVICE FILES
-------------

If you ever change any of these files, issue `consul reload` to apply
the changes.

* `/etc/consul/conf.d/*.service`
