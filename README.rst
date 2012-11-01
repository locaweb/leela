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

API Stability
=============

* testing [2]_

.. [2] Possible values:

       * stable: production ready;
       * testing: somehow ready, but not yet ready for production;
       * unstable: do not use, untested, API changing very often;

Version
=======

* $version 1.0.0$

License
=======

APACHE 2.0

Dev Bootstrap
=============

Dependencies
------------

* python              [2.6 <= x < 3]

  * argparse

  * telephus          [>= 1.0      ]

  * wokkel            [>= 0.7.0    ]

  * txredisapi        [>= 0.9      ]

  * twisted           [>= 10.1     ]

* python-testing

  * nose

  * mock

* haskell

  * attoparsec        [>= 0.10.2   ]

  * blaze-builder     [>= 0.3.1    ]

  * double-conversion [>= 0.2.0.5  ]

  * regex-tdfa        [>= 1.1.8    ]

  * stm               [>= 2.4      ]

  * hslogger          [>= 1.2.1    ]

* haskell-testing

  * hspec

  * quickcheck

Bootstrapping
-------------

Prepares the development environment. Make sure you have ``ghc [>=
7]``, ``cabal``, ``python [>= 2.6]`` and *python header files*. If you
are using debian, the following should suffice::

  $ apt-get install ghc=7.* cabal python=2.7.* python-dev=2.7.* python-virtualenv

Make sure you get these right for your platform. Now, issue::

  $ make -f dev.makefile bootstrap

And you are done.

Compiling
---------
::

  $ make -f dev.makefile compile

Testing
-------
::

  $ make -f dev.makefile test


Golden Tests
------------

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

  make -f dev.makefile test-golden
