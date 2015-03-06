# Development Environment

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

```.shell
$ git clone git://github.com/locaweb/leela.git
$ cd leela
```

We can now create the docker images we need. To see what images are
available, issue the command with no arguments.

```.shell
$ sudo ./automation/docker/makeimg.sh debian7.amd64
```

This may take a while, but after it is done you should be able to see
a new container available:


    $ sudo docker images
    REPOSITORY             TAG                 IMAGE ID            CREATED             VIRTUAL SIZE
    leela/debian7-amd64    latest              2e01277ea2ca        6 hours ago         525.1 MB


This completes the foundations. From here it really depends on what
you want to do. Each component has its own set of dependencies and the
following sections details each one of them.

## Client Libraries

This section covers _libleela_, _libleela-python_ and
__libleela-ruby_.

### Compiling

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

```.shell
docker $ env distroot=/tmp/leela/dist \
           /leela/automation/devel/envleela COMMAND
```

Just keep in mind that you must use the very same ``distroot`` to
build leela components as often the components depends on each other.

#### libleela

```.shell
$ sudo docker run \
         --rm \
         -i -t \
         -v $(pwd):/leela \
       leela/debian7-amd64 /bin/bash
docker $ /leela/automation/bootstrap/zeromq-bootstap.sh
docker $ make -C /leela/automation/devel compile.libleela
```

This should install files under the ``distroot`` variable which in
this case we defined to _/tmp/leela/dist_. Your lib directory should
have these files:

```.shell
$ ls -1F /tmp/leela/dist/lib/
libleela.so@
libleela.so.6@
libleela.so.6.4.1
libpoly1305aes.a
```

#### libleela-python

If everything went OK you can build the _libleela-python_:

```.shell
docker $ make -C /leela/automation/devel compile.libleela-python
```

This installs the pyleela python module:

```.shell
docker $ /leela/automation/devel/envleela python2 -c 'import pyleela.lql'
```

#### libleela-ruby

TODO:fixme
