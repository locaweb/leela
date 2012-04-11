# leela - Collect and Chart anything

## What is it? How it works?
Leela is a framework to collect and chart whatever you want :D
it uses a pretty simple protocol encapsulated on bzip compression,
you can easily use the python-leela or ruby-leela lib to send data
or even a simple awk | bzip | nc script

## Depends:
* cassandra
* pycassa
* supay
* gevent => 0.13.6
* bottle => 0.10

## Chart example:
<img src="https://github.com/ncode/leela-server/blob/master/example.png">

## Usage:
### Running the server
    $ src/sbin/leela-server -a foreground -c python/etc/leela.conf &
    $ bash src/share/examples/cpu.sh
