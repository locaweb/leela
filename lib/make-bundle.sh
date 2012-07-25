#!/bin/sh

java -jar lib/yuicompressor-2.4.7.jar usr/share/leela/static/js/widget.js >/tmp/leela-bundle.js
/bin/cat usr/share/leela/static/js/highcharts.js usr/share/leela/static/js/modules/exporting.js /tmp/leela-bundle.js >usr/share/leela/static/js/leelaserver-bundle.js
