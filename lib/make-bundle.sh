#!/bin/sh

java -jar lib/yuicompressor-2.4.7.jar src/share/static/js/widget.js >/tmp/leela-bundle.js
/bin/cat src/share/static/js/highcharts.js src/share/static/js/modules/exporting.js /tmp/leela-bundle.js >src/share/static/js/leelaserver-bundle.js
