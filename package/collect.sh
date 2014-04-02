#!/bin/sh

mkdir -p "$distdir"
for t in "$@"
do
  cp -vr "$t" "$distdir"
done
