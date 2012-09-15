#!/bin/bash

if [ "$1" == "full" ]
then
  nodetool -h localhost repair
  nodetool -h localhost compact
  nodetool -h localhost cleanup
else
  nodetool -h localhost repair -pr
fi
