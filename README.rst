=====
LEELA
=====

Leela is a server that allows you to collect any kind of metrics,
following loosely the statsd protocol.

It is also enables you to perform some basic monitoring on the
metrics, to notify when something unexpected has happened.

It is written in python and uses cassandra as the default storage
backend.

For more information about the usage and architecture please refer to
the documentation.

Documentation
=============

* http://leela-server.readthedocs.org/

API Stability
=============

* testing [2]_

.. [2] Possible values:

       * stable: production ready;
       * testing: somehow ready, but not yet ready for production;
       * unstable: do not use, untested, API changing very often;

License
=======

APACHE 2.0
