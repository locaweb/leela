Leela is a multi-language project and getting the development
environment right is not a trivial task. But fear not my friend, much
has been done to make this task simpler.

There are many ways of achieving this but in this document we have
decided to use docker [^1]. Docker allows you to
create and manage linux containers [well, it actually is much more
than that, but it is outside the scope of this document to go into
further details], which we will use to create an isolated
environment where we can compile, test and execute Leela.

[^1]: http://docker.io

Lets first clone the project:

```.bash
$ git clone git://github.com/locaweb/leela.git
$ cd leela
```

We can now create the docker images we need. To see what images are
available, issue the command with no arguments.

```.bash
$ sudo ./automation/docker/makeimg.sh debian7.amd64
```

Notice there are more images available. Just issue the same command
without arguments in order to know what is available. For instance,
there are centos images available too.

This may take a while, but after it is done you should be able to see
a new container available:

    $ sudo docker images
    REPOSITORY             TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
    leela/debian7-amd64    latest              2e01277ea2ca        6 hours ago         525.1 MB


This complete the foundation. From here it really depends on what you
want to do. Each component has its own set of dependencies and the
following sections details each one of them. However, without going
into much detail, this table summarizes what needs to be done:

| component       | bootstrap-scripts     |
|-----------------|---------------------- |
| libleela        | zeromq-bootstrap.sh   |
| libleela-python | zeromq-bootstrap.sh   |
|                 | python-bootstrap.sh   |
| libleela-ruby   | zeromq-bootstrap.sh   |
|                 | ruby-bootstrap.sh     |
| collectd        | zeromq-bootstrap.sh   |
|                 | collectd-bootstrap.sh |
| warpdrive       | zeromq-bootstrap.sh   |
|                 | haskell-bootstrap.sh  |
| blackbox        | zeromq-bootstrap.sh   |
|                 | jzmq-bootstrap.sh     |
|                 | clojure-bootstrap.sh  |
| warpgrep        | zeromq-bootstrap.sh   |
|                 | haskell-bootstrap.sh  |

Notice that you must prepare the environment according to the next
section before compiling or testing the modules.

# PREPARE THE ENVIRONMENT

This section contains what must be done to prepare the environment for
compiling Leela. Each module gets its own section and its
self-contained and the title matches the names in the following
sections.

## LIBLEELA

```.bash
debian7.amd64 $ /leela/automation/bootstrap/zeromq-bootstrap.sh
```

## LIBLEELA-PYTHON

```.bash
debian7.amd64 $ /leela/automation/bootstrap/python-bootstrap.sh
debian7.amd64 $ /leela/automation/bootstrap/zeromq-bootstrap.sh
```

## LIBLEELA-RUBY

TODO:fixme

## WARPDRIVE|WARPGREP

```.bash
debian7.amd64 $ /leela/automation/bootstrap/zeromq-bootstrap.sh
debian7.amd64 $ /leela/automation/bootstrap/haskell-bootstrap.sh
```

## BLACKBOX

```.bash
debian7.amd64 $ /leela/automation/bootstrap/zeromq-bootstrap.sh
debian7.amd64 $ /leela/automation/bootstrap/clojure-bootstrap.sh
debian7.amd64 $ /leela/automation/bootstrap/jzmq-bootstrap.sh
```

# CLIENT LIBRARIES

This section covers _libleela_, _libleela-python_ and
__libleela-ruby_.

There is a _makefile_ on _automation/devel_ directory that has code to
compile the libraries on linux. This _makefile_ uses two importante
variables when building:

* ``buildroot``: the directory to store build generated
  files. Defaults to _/tmp/leela/build_;

* ``distroot``: the installation directory. Defaults to
  _/tmp/leela/dist_;

Since these directories are not standard the linker will probably fail
to find libraries. The same goes for Python. For this reason, there is
a simple script that defines a couple of environment variables that
enables you to use this non standard path:

```.bash
debian7.amd64 $ env distroot=/tmp/leela/dist \
                  /leela/automation/devel/envleela COMMAND
```

Just keep in mind that you must use the very same ``distroot`` to
build leela components as often the components depends on each other.

## LIBLEELA

```.bash
$ sudo docker run \
         --rm \
         -i -t \
         -v $(pwd):/leela \
       leela/debian7-amd64 /bin/bash
debian7.amd64 $ /leela/automation/bootstrap/zeromq-bootstap.sh
debian7.amd64 $ make -C /leela/automation/devel compile.libleela
```

This should install files under the ``distroot`` directory:

```.bash
$ ls -1F /tmp/leela/dist/lib/
libleela.so@
libleela.so.6@
libleela.so.6.4.1
libpoly1305aes.a
```

## LIBLEELA-PYTHON

```.bash
debian7.amd64 $ make -C /leela/automation/devel compile.libleela-python
```

This installs the *pyleela* python module:

```.bash
debian7.amd64 $ /leela/automation/devel/envleela python2 -c 'import pyleela.lql; print("ok");'
```

## LIBLEELA-RUBY

TODO:fixme

## COLLECTD

```.bash
debian7.amd64 $ make -C /leela/automation/devel compile.collectd
```

Which must produce the write_leela shared library:

```.bash
debian7.amd64 $ ls -1 /tmp/leela/lib/write_leela.so
/tmp/leela/lib/write_leela.so
```

# CORE MODULES

## BLACKBOX

```.bash
debian7.amd64 $ make -C /leela/automation/devel compile.blackbox
```

## WARPGREP

```.bash
debian7.amd64 $ make -C /leela/automation/devel compile.warpgrep
```

# FRONTEND

## WARPDRIVE

```.bash
debian7.amd64 $ make -C /leela/automation/devel compile.warpdrive
```
