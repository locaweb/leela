This section contains information about how to install a Leela
cluster. Is is divided into two sections. The first covers how to
create the packages and the later how to install and configure the
packages.

This document will use _debian-wheezy_ as the target platform and will
make use of the docker image that has been described in the
[environment](../devel/environment.md) section. If you haven't
done so refer to that document to get fetch the source code and create
the docker image.

Now start the container. We will use it to build every single
component:

```.bash
$ docker run \
    -v .:/leela \
    --rm -i -t \
    leela/debian7-{arch} /bin/bash
```

Also this guide will install the local packages we have just
generated. You are free to use any frontend you like but as we are
using local files we find it easier using _gdebi_.

```.bash
$ apt-get install gdebi
```

## LIBLEELA

Libleela is the client library written in C. It is the base for the
other libraries written in other languages and also contain the
poly1305aes library. You will need it to built other leela components.

```.bash
docker $ make -C /leela/pkg libleela.debian
```

It is likely to fail since there might be a few dependencies
missing. However those can be satisfied by the package manager. So
just install those that are missing and try again.

On success the package will be available at:

```.bash
docker $ ls -1 /leela/pkg/dist/debian7/{arch}/libleela/
libleela-dev_{major}.{minor}.{patch}-{build}_{arch}.deb
libleela_{major}.{minor}.{patch}-{build}.dsc
libleela_{major}.{minor}.{patch}-{build}.tar.gz
libleela_{major}.{minor}.{patch}-{build}_{arch}.changes
libleela_{major}.{minor}.{patch}-{build}_{arch}.deb
```

## LIBLEELA-PYTHON

```.bash
docker $ make -C /leela/pkg libleela-python.debian
```

Likewise, some dependencies may be missing. You must solve them and
try again. If everything went right, you should have the following
packages:

```.bash
docker $ ls -1 /leela/pkg/dist/debian7/{arch}/libleela-python
libleela-python_x.y.z-b.dsc
libleela-python_x.y.z-b.tar.gz
libleela-python_x.y.z-b_{arch}.changes
libleela-python_x.y.z-b_{arch}.deb
```

## WARPDRIVE

The warpdrive is the frontend of the Leela cluster. It is the service
that allows clients to store data and retrieve
information.

```.bash
docker $ make -C /leela/pkg leela-warpdrive.debian
```

Which should generate the following files:

```.bash
docker $ ls -1 /leela/pkg/dist/debian7/{arch}/leela-warpdrive/
leela-warpdrive_{major}.{minor}.{patch}-{build}.dsc
leela-warpdrive_{major}.{minor}.{patch}-{build}.tar.gz
leela-warpdrive_{major}.{minor}.{patch}-{build}_{arch}.changes
leela-warpdrive_{major}.{minor}.{patch}-{build}_{arch}.deb
```

## COLLECTD

This package provides a collectd write plugin. With this you can send
collectd metrics to Leela.

As usual, lets start preparing the environment. Notice that we use
collectd from source, since it is a bit unusual to compile modules.

## BLACKBOX

This is the storage backend used by the Leela cluster. It is used
internally and it the module that gives access to the cassandra
cluster.

It is written in clojure which means you will need java
[we recommend using Oracle's version, specially in production]. We
will use an Ubuntu PPA repository since debian has no java packages
but openjdk.

```.bash
docker $ /leela/automation/bootstrap/clojure-bootstrap.sh
```

After this script finishes you will have java and leiningen installed
which allows us to build the package:

```.bash
docker $ make -C /leela/pkg leela-blackbox.debian
```

Afterthat you should have the following files:

```.bash
docker $ ls -1 /leela/pkg/dist/debian7/{arch}/leela-blackbox/
leela-blackbox_{major}.{minor}.{patch}-{build}.dsc
leela-blackbox_{major}.{minor}.{patch}-{build}.tar.gz
leela-blackbox_{major}.{minor}.{patch}-{build}_{arch}.changes
leela-blackbox_{major}.{minor}.{patch}-{build}_{arch}.deb
```
# INSTALL

## BLACKBOX

First install the package:

```.bash
$ gdebi install leela/pkg/leela-blackbox_{major}.{minor}.{patch}-{build}_{arch}.deb
```

Then you must configure the daemon. The file that must be changed is
``/etc/default/leela-blackbox``. It is self-documented, so we just
highlight the most important options:

The credentials to connect to cassandra cluster:

```.bash
LEELA_BLACKBOX_USERNAME=leela
LEELA_BLACKBOX_PASSWORD=...
```

Then the cassandra cluster endpoints. Use more than one, probably the
seed nodes are a good option:

```.bash
LEELA_BLACKBOX_CASSANDRA=127.0.0.1,127.0.0.2
```

## WARPDRIVE

First install the package:

```.bash
$ gdebi install leela/pkg/leela-warpdrive_{major}.{minor}.{patch}-{build}_{arch}.deb
```

Then you must configure the daemon. The file you must change on debian
is ``/etc/default/leela-warpdrive``. It is self-documented so we just
describe the most important options.

The following two variables defines the username and password used to
test this ``warpdrive`` instance. Leela uses consul for auto-discovery
and each service has a health checker associated. If the watchdog
script fails to execute the instance does not get announced.

```.bash
LEELA_WARPDRIVE_WATCHDOG_USER=
LEELA_WARPDRIVE_WATCHDOG_PASS=
```

Another very important configuration is:

```.bash
LEELA_WARPDRIVE_ENDPOINT=
```

It defines network address of this instance that will be announced in
consul. You probably will need to change it since it defaults to
local host.

After the ``/etc/default/leela-warpdrive`` properly configured, you
may start the daemon:

```.bash
$ /etc/init.d/leela-warpdrive [start|stop|restart]
```

Notice that it depends on a number of services to work correcly. Make
sure you got these dependencies satisfied before making your cluster
available:

* redis [[install guide](install-redis.md)]
* consul [[install guide](install-consul.md)]
* blackbox [[install guide](install-leela.md#installing-blackbox)]
