==========
 Overview
==========

Leela is an event processing, analysis and monitoring system:

* *event processing* [stream processor is used a synonym in this
  document] is the ability to apply a function over a set of events,
  for instance, *average* or *max*;

* *monitoring* is the task of applying any *stream processor* to real
  time events, as they arrive, allowing one to report anomalies, like
  cpu usage, or make sure a given service is up and running;

* *analyzing* is hereby defined as the task of retrieving stored data
  [say the past 6 months] and, optionally, applying a stream processor
  over this data. As an example, suppose you want the past year data
  but with a lower resolution.
