# leela - Collect and Chart anything



## What is it? How it works?
Leela is a framework to collect and chart whatever you want :D
it uses a pretty simple protocol encapsulated on bzip compression,
you can easily use the python-leela or ruby-leela lib to send data
or even a simple awk | bzip | nc script

## Simple Protocol
    about_who|chart_name||field|value||field|value
    karoly|memory||virtmem_usage|128||phymem_usage|998

## Depends:
* cassandra
* pycassa
* supay
* gevent => 0.13.6
* bottle => 0.10
* python-cheetah

## Chart example:
<img src="https://github.com/locaweb/leela-server/raw/master/example.png">

## Usage:
### Running the server
    $ src/sbin/leela-server -a foreground -c python/etc/leela.conf &
    $ bash src/share/examples/cpu.sh

## Javascript widget (requires jquery)
    <div id="canvas" />
    <script src="<endpoint>/static/js/leelaserver-bundle.js" type="text/javascript" />
    <script type="text/javascript">
      jQuery.ajax("<endpoint>/json/:hostname/:service/:time", { dataType: "jsonp",
                                                                success: LEELA.widget("canvas").install
                                                              });
    </script>
