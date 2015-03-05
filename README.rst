=====
LEELA
=====

Leela is a simple, but scalable, property-graph database where
properties can be either time-series or key-value.

It was created to store information about about our datacenter and its
many clients like applications and users of those applications. The
properties provides an additional layer of information that allows to
store historical data about entities like servers, supporting our
monitoring system.

For an example, these entities can be easily represented in leela:
::


      o cpu-usage [time-series]
      |  
      |  o hwinfo [json-data]
      |  |
   +---------+                        +--------+
   | machine |----------------------->| switch |
   +---------+                        +--------+
                                           |
           +------------+    +-----+       |
           | datacenter |<---| rak |<------+
           +------------+    +-----+

Development
===========

Since leela has many subprojects, you need to solve a lot of
dependencies in order to setup the development environment.

Fear not my friend, for we have created a couple of scripts to assist
you in this task.

First, there are two possible paths:

  1. use docker and create the base images;

  2. use your own machine, which must be either centos or debian;

The following creates the dev environment using docker, since we find
using it easier.

1. Create the base images:

   
    # the following images are available:
      * debian[67].(amd64|i386)
      * centos[56].(amd64|i386)
    $ sudo ./automation/docker/makeimg.sh debian7.amd64

2. zeromq is required for anything, so we start with it:


   $ sudo docker run --rm -v $(pwd):/leela -i -t leela/debian-7-amd64 /bin/bash
   # /leela/automation/bootstrap/zeromq-bootstrap.sh

This allows you to compile ``libleela``, ``libleela-python`` and
``libleela-ruby``.

The other scripts set up dependencies for other components. The
following table explains in details these relationships:


    +==================+=====================+
    | component        | bootstrap script    |
    +==================+=====================+
    | libleela         |   zeromq-bootstrap  |
    +------------------+---------------------+
    | libleela-python  |   zeromq-bootstrap  |
    +------------------+---------------------+
    | libleela-ruby    |   zeromq-bootstrap  |
    +------------------+---------------------+
    | warpdrive        |   zeromq-bootstrap  |
    |                  |   haskell-bootstrap |
    +------------------+---------------------+
    | warpgrep         |   zeromq-bootstrap  |
    |                  |   haskell-bootstrap |
    +------------------+---------------------+
    | blackbox         |   zeromq-bootstrap  |
    |                  |   jzmq-bootstrap    |
    |                  |   clojure-bootstrap |
    +------------------+---------------------+

These scripts were tested using debian and centos images. But they
would probably work in other distros as well, like arch linux.

Packaging
=========

This assumes you followed the procedure described in the Development
section.

    $ make -C /leela/package libleela.debian7 arch=amd64
    $ make -C /leela/package libleela-python.debian7 arch=amd64

The packages are stored under the ``/leela/package/dist/`` directory.

Contribute
==========

Any help is welcome and there is no formal process. Just remember:

  * use good commit messages and provide a good description about what
    you are trying to achieve;
  * make sure your patch applies cleanly;
  * include/update tests;
  * update the documentation;

Documentation
=============

* http://leela.rtfd.org/

License
=======

APACHE-2.0

Author
======

* dgvncsz0f <dsouza@c0d3.xxx>

Contributors
============

* Luiz Ozaki
* Rodrigo Vaz
* Andre Ferraz
* Juliano Martines [former author (v0.0.9)]
* Willian Mollinari

