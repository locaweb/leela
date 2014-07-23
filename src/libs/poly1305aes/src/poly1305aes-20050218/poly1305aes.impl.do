# poly1305aes.impl.do version 20050218
# D. J. Bernstein
# Public domain.

echo '#include "poly1305aes_athlon.h"' > poly1305aes.impl.check.h
if gcc -o x86cpuid x86cpuid.c >/dev/null 2>&1 \
&& ./x86cpuid > x86cpuid.out \
&& grep '^AuthenticAMD' x86cpuid.out >/dev/null 2>&1 \
&& $* -o poly1305aes.impl.check poly1305aes.impl.check.c \
aes_athlon.s aes_athlon_constants.s \
poly1305_athlon.s poly1305_athlon_constants.s \
poly1305aes_athlon_authenticate.c poly1305aes_athlon_verify.c \
poly1305aes_athlon_clamp.c poly1305aes_athlon_isequal.s >/dev/null 2>&1 \
&& ./poly1305aes.impl.check
then
  echo athlon
  exit 0
fi

echo '#include "poly1305aes_ppro.h"' > poly1305aes.impl.check.h
if $* -o poly1305aes.impl.check poly1305aes.impl.check.c \
aes_ppro.s aes_ppro_constants.s \
poly1305_ppro.s poly1305_ppro_constants.s \
poly1305aes_ppro_authenticate.c poly1305aes_ppro_verify.c \
poly1305aes_ppro_clamp.c poly1305aes_ppro_isequal.s >/dev/null 2>&1 \
&& ./poly1305aes.impl.check
then
  echo ppro
  exit 0
fi

echo '#include "poly1305aes_macos.h"' > poly1305aes.impl.check.h
if $* -o poly1305aes.impl.check poly1305aes.impl.check.c \
aes_macos.s aes_macos_constants.s \
poly1305_macos.s poly1305_macos_constants.s \
poly1305aes_macos_authenticate.c poly1305aes_macos_verify.c \
poly1305aes_macos_clamp.c poly1305aes_macos_isequal.s >/dev/null 2>&1 \
&& ./poly1305aes.impl.check
then
  echo macos
  exit 0
fi

echo '#include "poly1305aes_aix.h"' > poly1305aes.impl.check.h
if $* -o poly1305aes.impl.check poly1305aes.impl.check.c \
aes_aix.s aes_aix_constants.s \
poly1305_aix.s poly1305_aix_constants.c \
poly1305aes_aix_authenticate.c poly1305aes_aix_verify.c \
poly1305aes_aix_clamp.c poly1305aes_aix_isequal.s >/dev/null 2>&1 \
&& ./poly1305aes.impl.check
then
  echo aix
  exit 0
fi

echo '#include "poly1305aes_sparc.h"' > poly1305aes.impl.check.h
if $* -o poly1305aes.impl.check poly1305aes.impl.check.c \
aes_sparc.s aes_sparc_constants.c \
poly1305_sparc.s poly1305_sparc_constants.c \
poly1305aes_sparc_authenticate.c poly1305aes_sparc_verify.c \
poly1305aes_sparc_clamp.s poly1305aes_sparc_isequal.s \
poly1305aes_sparc_fsr.s >/dev/null 2>&1 \
&& ./poly1305aes.impl.check
then
  echo sparc
  exit 0
fi

echo '#include "poly1305aes_53.h"' > poly1305aes.impl.check.h
if $* -o poly1305aes.impl.check poly1305aes.impl.check.c \
aes_big.c aes_big_constants.c \
poly1305_53.c poly1305_53_constants.c \
poly1305aes_53_authenticate.c poly1305aes_53_verify.c \
poly1305aes_53_clamp.c poly1305aes_53_isequal.c >/dev/null 2>&1 \
&& ./poly1305aes.impl.check
then
  echo 53
  exit 0
fi

echo 'poly1305aes.impl.do: fatal: all tests failed!' >&2
exit 1
