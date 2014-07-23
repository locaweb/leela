# poly1305aes.h.do version 20050218
# D. J. Bernstein
# Public domain.

case `cat poly1305aes.impl` in
  53)     echo '#include "poly1305aes_53.h"' ;;
  aix)    echo '#include "poly1305aes_aix.h"' ;;
  athlon) echo '#include "poly1305aes_athlon.h"' ;;
  macos)  echo '#include "poly1305aes_macos.h"' ;;
  ppro)   echo '#include "poly1305aes_ppro.h"' ;;
  sparc)  echo '#include "poly1305aes_sparc.h"' ;;
  *) echo 'unknown implementation' >&2; exit 1 ;;
esac
