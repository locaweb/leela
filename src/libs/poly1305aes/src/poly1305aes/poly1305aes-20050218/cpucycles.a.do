# cpucycles.a.do version 20050218
# D. J. Bernstein
# Public domain.

rm -f cpucyclestmp.a

impl=`cat poly1305aes.impl`

case $impl in
  53)
    $* -c cpucycles_zero.c
    ar cr cpucyclestmp.a cpucycles_zero.o
    ;;
  aix|athlon|macos|ppro|sparc)
    $* -c cpucycles_${impl}.s
    ar cr cpucyclestmp.a cpucycles_${impl}.o
    ;;
  *) echo 'unknown implementation' >&2; exit 1 ;;
esac

ranlib cpucyclestmp.a >/dev/null 2>/dev/null || :
cat cpucyclestmp.a
rm cpucyclestmp.a
