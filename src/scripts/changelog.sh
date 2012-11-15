#!/bin/sh

leela_root=${leela_root:-$(pwd)}
bin_sed=${bin_sed:-/bin/sed}

gitlog () {
  git --no-pager log "$@"
}

header () {
  echo CHANGELOG
  echo =========
}

prepend () {
  src=$1
  chk=$2

  mv "$src" "$src.0"    && \
    mv "$chk" "$src"    && \
    cat "$src.0" >>"$src"
  rm -f "$src.0" "$chk"
}

payload () {
  tag0=$(git --no-pager rev-list --max-parents=0 HEAD)
  for tag in $(git tag | grep '^v' | sort -V)
  do
    hdr=$(gitlog -n1 --pretty=format:"$tag | (%ai)" $tag)
    echo                        >"$TMPFILE.1"
    echo "$hdr"                >>"$TMPFILE.1"
    echo "$hdr" | sed s/./-/g  >>"$TMPFILE.1"

    for commit in $tag0 $(gitlog --pretty=format:"%H" $tag0..$tag)
    do
      prefix=$(gitlog -n1 --pretty=format:%s $commit | sed s,/,,g)
      suffix=$(gitlog -n1 --pretty=format:"| %aN (%ai)" $commit | sed s,/,,g)
      gitlog -n1 --pretty=format:"%b%n%N" $commit    | \
        grep '^\[changelog\]'                  | \
        sed -r "s/^\\[changelog\\] *\$/  * $prefix/; s/^\\[changelog\\] /  * /; s/\$/ $suffix/"
    done >>"$TMPFILE.1"

    prepend "$TMPFILE" "$TMPFILE.1"
    tag0=$tag
  done
}

TMPFILE=`mktemp -t changelog.XXXXXXXXXX` && {
  payload "$TMPFILE"
  header >"$TMPFILE.1"

  prepend "$TMPFILE" "$TMPFILE.1"
  cat "$TMPFILE"
  rm -f "$TMPFILE"
}
