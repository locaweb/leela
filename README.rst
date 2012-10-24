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

* python              [2.6.x     ]

  * argparse

  * telephus          [>= 1.0    ]

  * wokkel            [>= 0.7.0  ]

  * txredisapi        [>= 0.9    ]

  * twisted           [>= 10.1   ]

* python-testing

  * nose

  * mock

* haskell

  * attoparsec        [>= 0.10.2 ]

  * blaze-builder     [>= 0.3.1  ]

  * double-conversion [>= 0.2.0.5]

  * regex-tdfa        [>= 1.1.8  ]

  * stm               [>= 2.4    ]

  * hslogger          [>= 1.2.1  ]

* haskell-testing

  * hspec

  * quickcheck

Bootstrapping
-------------

Prepares the development environment. Uses cabal for haskell and
virtualenv for python::

  $ make -f dev.makefile bootstrap

Compiling
---------
::
  
  $ make -f dev.makefile compile

Testing
-------
::
  
  $ make -f dev.makefile test


