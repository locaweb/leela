# aes.h.do version 20050218
# D. J. Bernstein
# Public domain.

case `cat poly1305aes.impl` in
  53)     echo '#include "aes_big.h"' ;;
  aix)    echo '#include "aes_aix.h"' ;;
  athlon) echo '#include "aes_athlon.h"' ;;
  macos)  echo '#include "aes_macos.h"' ;;
  ppro)   echo '#include "aes_ppro.h"' ;;
  sparc)  echo '#include "aes_sparc.h"' ;;
  *) echo 'unknown implementation' >&2; exit 1 ;;
esac
