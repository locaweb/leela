REDIS
=====

Redis operation guide. Redis is used to reduce the number of writes in
cassandra and to reply *last* queries.

MAINTENANCE
-----------

1. `/etc/init.d/leela-redis watchdog-stop; sleep 60`  # leela will no longer use this machine
2. `/etc/init.d/leela-redis stop`                     # stops the service; now both redis are down
3. \# do your thing
4. `/etc/init.d/leela-redis start`

ADDING INSTANCE
---------------

1. make sure the file `/etc/consul/conf.d/redis.service` exists and is
properly configured. This file should come from CFengine.
2. `/etc/init.d/leela-redis start`

REMOVING INSTANCE
-----------------

1. `/etc/init.d/leela-redir watchdog-stop; sleep 60`

CLUSTER INFO
------------

* `curl -s "http://webleela.service.ita.consul.locaweb.com.br/v2?q=using%20(::)%20stat;&tree=locaweb::locaweb&format=json" \
    | python -m json.tool | grep redis -A1`

Each of these endpoints must have two redis. The second redis must be
listening at `port + 1`. For instance, if redis bound to 6379, the
second one must be listening at 6380.

This second redis must be a slave of the first in a master-slave
replication configuration. It is used to serve read queries to avoid
blocking the master. As you know, redis is single threaded and we
don't want the reads negatively impacting the write path.
