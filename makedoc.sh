#!/bin/sh

set -e

branch=${branch:-origin/master}

mkdocs_bin=${mkdocs_bin:-mkdocs}

buildroot=$(mktemp -d) && {
  trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
  git fetch; git fetch --tags
  git archive --format=tar --prefix=leela/ "$branch" | tar -C "$buildroot" -x
  tag=$(git describe --long --tags "$branch")
  sed -i "s/2.0.0/$tag/g" "$buildroot/leela/doc/mkdocs.yml"
  (cd "$buildroot/leela/doc" && "$mkdocs_bin" build)
  rm -rf doc
  mv "$buildroot/leela/doc/site/" doc/
  rm -rf "$buildroot"
}
