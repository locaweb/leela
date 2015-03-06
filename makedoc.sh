#!/bin/sh

set -e

buildroot=$(mktemp -d) && {
  trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
  git archive --format=tar --prefix=leela/ ${branch:-master} | tar -C "$buildroot" -x
  (cd "$buildroot/leela/doc" && "${mkdocs_bin:-mkdocs}" build)
  rm -rf site
  mv "$buildroot/leela/doc/site/" site/
  rm -rf "$buildroot"
}
