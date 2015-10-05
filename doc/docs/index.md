# Leela

## Introduction

Leela started as a time-series engine, very similar to graphite [^1]
but with less features, tailored for our needs. We found it hard to
scale graphite at the time and figure that using cassandra would make
it a lot easier to scale up and cope with our load. Back then we were
collecting mostly server metrics like cpu, memory and disk.

[^1]: http://graphite.readthedocs.org

After a while we figure we also wanted to store more information about
our datacenter. Like the relationships a machine or switch may
have. For instance, in what switch a given network card is connected
to.

Leela then evolved into a property-graph engine. It continue to be a
time-series storage but we now could create vertexes and connects them
to each other.

This allowed us to store complex data, like the relationships we have
in our datacenter:

```
      o cpu-usage [time-series]
      |  
      |  o hwinfo [json-data]
      |  |
   +---------+     +-----+            +--------+
   | machine |-----| nic |----------->| switch |
   +---------+     +-----+            +--------+
                                          |
           +------------+    +-----+      |
           | datacenter |<---| rak |<-----+
           +------------+    +-----+
```

It is an ever-evolving system and we are continuously working on
it. New features are planned and we are working hard to improve the
documentation as fast as we can.

## The Documentation

This section describes how the documentation has been organized. There
are three major sections, Development, Operations and Users. Each
section covers a given aspect of the system from that perspective.

Orthogonal to those sections there are the various modules that
comprise Leela.

| client libraries | backend modules | frontend   |
|------------------|-----------------|------------|
| libleela         | warpgrep        | warpserver |
| libleela-ruby    | blackbox        | webleela   |
| libleela-python  |                 |            |

All client libraries are built on top of *libleela*. *Libleela* is
written in C and the other languages creates a module on top of that
library.

*Blackbox* and *warpgrep* are internal modules, the user usually don't
need to interact with them. They are used by *warpserver*. The former
provides access to the storage engine whereas the later allows users
to monitor queries in real-time.

*Warpserver* and *webleela* are the frontends. They provide a *ZMQ*
 interface and an *HTTP* interface, respectively.

### Development

The audience is software developers of ones interested in contributing
or learning about Leela internals.

* [Architecture](devel/architecture.md)

* [Environment](devel/environment.md)

* [Network Protocol](devel/network-protocol.md)

* [Security](devel/security.md)

### Operations

The audience is system administrators or ones interested in
installing, maintaining and tuning a Leela cluster.

* [Packaging Leela](admin/packaging-leela.md)

* [Cassandra](admin/cassandra.md)

* [Consul](admin/consul.md)

* [Redis](admin/redis.md)

* [Installing Leela](admin/install-leela.md)

* [Tuning Leela](admin/tuning-leela.md)

### User Guide

For the ones interested in using a Leela cluster.

* [Leela Query Language](user/leela-query-language.md)

* [ZMQ Interface](user/zeromq-interface.md)

* [HTTP Interface](user/http-interface.md)
