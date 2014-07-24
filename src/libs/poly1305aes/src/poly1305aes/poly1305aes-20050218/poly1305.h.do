# poly1305.h.do version 20050218
# D. J. Bernstein
# Public domain.

case `cat poly1305aes.impl` in
  53)     echo '#include "poly1305_53.h"' ;;
  aix)    echo '#include "poly1305_aix.h"' ;;
  athlon) echo '#include "poly1305_athlon.h"' ;;
  macos)  echo '#include "poly1305_macos.h"' ;;
  ppro)   echo '#include "poly1305_ppro.h"' ;;
  sparc)  echo '#include "poly1305_sparc.h"' ;;
  *) echo 'unknown implementation' >&2; exit 1 ;;
esac
