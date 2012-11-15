.. image:: http://locastyle.locaweb.com.br/img/logoLocaweb.png

=====
LEELA
=====

Distributed, real time event processor and monitoring engine.

For information on the usage, architecture and much more please refer
to the documentation.

Documentation
=============

* http://leela.rtfd.org/

Installing from source
======================

unstable
--------
::

  $ pip install -r https://raw.github.com/locaweb/leela-server/master/PYDEPS.txt
  $ pip install https://github.com/locaweb/leela-server/archive/master.tar.gz

Version
=======

* $version 2.0.0$

License
=======

APACHE 2.0

Dev Bootstrap
=============
::

  $ make bootstrap

Compiling
---------
::

  $ make compile

Testing
-------
::

  $ make test


Smoke Tests
-----------

Acceptance testing.

For these to work, you will need the following
up and running:

* cassandra /localhost:9160;

* prosody   /localhost:5222;

* redis     /localhost:6379;

Additionally:

1. Create the schema on cassandra (refer to doc/cassandra.schema);

2. Use cassandra with no authentication;

3. Use redis with no authentication;

4. Register two xmpp users:

   a. foobar@localhost (pass: foobar)

   b. foobaz@localhost (pass: foobaz)

GoOd LuCk!

::

  $ make test-smoke
