#!/bin/bash

if [ "$1" == "full" ]
then
  nodetool -h localhost repair
else
  nodetool -h localhost repair -pr
fi
nodetool -h localhost compact
nodetool -h localhost cleanup
