#!/bin/dash

spec_files () {
  find . -type f -name \*Spec.hs
}

read_module () {
  grep '^module ' $1 | cut -d" " -f2
}

read_specs () {
  grep '[^ ].*:: *Spec *$' $1 | grep -v '^\s*--' | cut -d":" -f1
}

imports () {
  imports="import Test.Hspec"
  for f in $(spec_files)
  do
    module=$(read_module $f)
    imports="${imports}\nimport ${module}"
  done
}

specs () {
  specs="hspec $ do"
  for f in $(spec_files)
  do
    module=$(read_module $f)
    specs="$specs\n  describe \"$module\" \$ do"
    for spec in $(read_specs $f)
    do
      specs="$specs\n    ${module}.${spec}"
    done
  done
}

imports
specs

[ -n "$3" ] && exec >$3
echo "module Main (main) where"
echo
echo "$imports"
echo
echo "main :: IO ()"
echo "main = $specs"

