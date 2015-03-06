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

This section describes how the documentation has been organized. We
tried to make each document as self-contained as possible without
boring the reader too much by reading the same thing everywhere.

With this in mind, it may be helpful to read the architecture document
before proceeding. It gives both an high-level and an in-depth vision
of the system and also presents the many terms and components that
will be referenced throughout the documents.

### Development

The audience is software developers of ones interested in contributing
or learning about Leela internals.

* [Architecture](devel-guide/architecture.md)

* [Environment](devel-guide/environment.md)

* [Network Protocol](devel-guide/network-protocol.md)

* [Security](devel-guide/security.md)

### Administration

The audience is system administrators or ones interested in
installing, maintaining and tuning a Leela cluster.

* [Installing Cassandra](admin-guide/install-cassandra.md)

* [Installing Consul](admin-guide/install-consul.md)

* [Installing Leela](admin-guide/install-leela.md)

* [Tuning Leela](admin-guide/tuning-leela.md)

### User Guide

For the ones interested in using a Leela cluster.

* [Leela Query Language](user-guide/leela-query-language.md)

* [ZMQ Interface](user-guide/zeromq-interface.md)

* [HTTP Interface](user-guide/http-interface.md)
