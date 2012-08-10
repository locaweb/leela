==========
 Protocol
==========

The protocol is fairly simple:
::

  <name>: <value> [timestamp]

:name: Any string, up to 50 characters;
:value: Any double value;
:timestamp: [optional] the unix timestamp you want to store this event. If you don't provide this value the server will use the current timestamp;

Example
=======

The following shell commands produces valid events:

::

  $ echo "leela.protocol: 0.75"
  $ echo "leela.protocol2: 0.78 3600"
