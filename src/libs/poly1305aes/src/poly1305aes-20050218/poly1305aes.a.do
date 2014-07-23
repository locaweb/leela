# poly1305aes.a.do version 20050218
# D. J. Bernstein
# Public domain.

rm -f poly1305aestmp.a

impl=`cat poly1305aes.impl`

case ${impl} in
  53)
    $* -c aes_big.c
    $* -c aes_big_constants.c
    $* -c poly1305_${impl}.c
    $* -c poly1305_${impl}_constants.c
    $* -c poly1305aes_${impl}_authenticate.c
    $* -c poly1305aes_${impl}_clamp.c
    $* -c poly1305aes_${impl}_isequal.c
    $* -c poly1305aes_${impl}_verify.c
    ar cr poly1305aestmp.a \
      poly1305aes_${impl}_verify.o \
      poly1305aes_${impl}_isequal.o \
      poly1305aes_${impl}_clamp.o \
      poly1305aes_${impl}_authenticate.o \
      poly1305_${impl}.o \
      poly1305_${impl}_constants.o \
      aes_big.o \
      aes_big_constants.o
    ;;
  aix)
    $* -c aes_${impl}.s
    $* -c aes_${impl}_constants.s
    $* -c poly1305_${impl}.s
    $* -c poly1305_${impl}_constants.c
    $* -c poly1305aes_${impl}_authenticate.c
    $* -c poly1305aes_${impl}_clamp.c
    $* -c poly1305aes_${impl}_isequal.s
    $* -c poly1305aes_${impl}_verify.c
    ar cr poly1305aestmp.a \
      poly1305aes_${impl}_verify.o \
      poly1305aes_${impl}_isequal.o \
      poly1305aes_${impl}_clamp.o \
      poly1305aes_${impl}_authenticate.o \
      poly1305_${impl}.o \
      poly1305_${impl}_constants.o \
      aes_${impl}.o \
      aes_${impl}_constants.o
    ;;
  athlon|macos|ppro)
    $* -c aes_${impl}.s
    $* -c aes_${impl}_constants.s
    $* -c poly1305_${impl}.s
    $* -c poly1305_${impl}_constants.s
    $* -c poly1305aes_${impl}_authenticate.c
    $* -c poly1305aes_${impl}_clamp.c
    $* -c poly1305aes_${impl}_isequal.s
    $* -c poly1305aes_${impl}_verify.c
    ar cr poly1305aestmp.a \
      poly1305aes_${impl}_verify.o \
      poly1305aes_${impl}_isequal.o \
      poly1305aes_${impl}_clamp.o \
      poly1305aes_${impl}_authenticate.o \
      poly1305_${impl}.o \
      poly1305_${impl}_constants.o \
      aes_${impl}.o \
      aes_${impl}_constants.o
    ;;
  sparc)
    $* -c aes_${impl}.s
    $* -c aes_${impl}_constants.c
    $* -c poly1305_${impl}.s
    $* -c poly1305_${impl}_constants.c
    $* -c poly1305aes_${impl}_authenticate.c
    $* -c poly1305aes_${impl}_clamp.s
    $* -c poly1305aes_${impl}_fsr.s
    $* -c poly1305aes_${impl}_isequal.s
    $* -c poly1305aes_${impl}_verify.c
    ar cr poly1305aestmp.a \
      poly1305aes_${impl}_verify.o \
      poly1305aes_${impl}_isequal.o \
      poly1305aes_${impl}_clamp.o \
      poly1305aes_${impl}_authenticate.o \
      poly1305aes_${impl}_fsr.o \
      poly1305_${impl}.o \
      poly1305_${impl}_constants.o \
      aes_${impl}.o \
      aes_${impl}_constants.o
    ;;
  *) echo 'unknown implementation' >&2; exit 1 ;;
esac

ranlib poly1305aestmp.a >/dev/null 2>/dev/null || :
cat poly1305aestmp.a
rm poly1305aestmp.a
