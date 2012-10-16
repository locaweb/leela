#!/bin/sh

leela_root=${leela_root:-$(pwd)}
bin_sed=${bin_sed:-/bin/sed}

gitlog () {
  git --no-pager log "$@"
}

header () {
  echo CHANGELOG
  echo =========
  echo
}

payload () {
  tag0=ef3490aef1454971feb48e8d61c86b27e9e53faf
  for tag in $(git tag | grep '^v' | sort -V)
  do
    hdr=$(gitlog -n1 --pretty=format:"$tag | (%ai)" $tag)
    echo 
    echo "$hdr"
    echo "$hdr" | sed s/./-/g

    for commit in $(gitlog --pretty=format:"%H" $tag0..$tag)
    do
      suffix=$(gitlog -n1 --pretty=format:"| %aN <%aE> (%ai)%n")
      gitlog --pretty=format:%b -n1 $commit  | \
        grep '^changelog:: '                 | \
        sed "s/^changelog:: /  * /;s/$/ $suffix/"
    done

    tag0=$tag
  done
  echo
}

header
payload
