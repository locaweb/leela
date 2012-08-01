=====
LEELA
=====

Leela is a project that allows you to collect and plot any kind of
metrics, following loosely the statsd protocol, actually similar to
the statsd project.

It is also enables you to perform some basic monitoring on the
metrics, to notify when something unexpected has happened.

It is written in python and uses cassandra as the default storage
backend.

For more information about the usage and architecture, comparison to
similar projects, please refer to the documentation.

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

Version
=======

* $version 0.0.1$

License
=======

APACHE 2.0
