#!/bin/bash

if [ "$1" == "full" ]
then
  nodetool -h localhost repair
  nodetool -h localhost compact
else
  nodetool -h localhost repair -pr
fi
nodetool -h localhost cleanup
