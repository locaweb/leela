# cpucycles.h.do version 20050218
# D. J. Bernstein
# Public domain.

case `cat poly1305aes.impl` in
  53)     echo '#include "cpucycles_zero.h"' ;;
  aix)    echo '#include "cpucycles_aix.h"' ;;
  athlon) echo '#include "cpucycles_athlon.h"' ;;
  macos)  echo '#include "cpucycles_macos.h"' ;;
  ppro)   echo '#include "cpucycles_ppro.h"' ;;
  sparc)  echo '#include "cpucycles_sparc.h"' ;;
  *) echo 'unknown implementation' >&2; exit 1 ;;
esac
