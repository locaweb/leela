#!/bin/sh

BUNDLE="usr/share/leela/static/js/highcharts.js usr/share/leela/static/js/modules/exporting.js /tmp/leela-bundle.js"

> /tmp/leela-bundle.js
for f in widget.js functions.js
do
  echo "compressing $f"
  java -jar lib/yuicompressor-2.4.7.jar usr/share/leela/static/js/$f >>/tmp/leela-bundle.js
done

> usr/share/leela/static/js/leelaserver-bundle.js
for f in $BUNDLE
do
  echo "bundling $f"
  cat $f >>usr/share/leela/static/js/leelaserver-bundle.js
done
