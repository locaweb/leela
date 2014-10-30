#ifndef poly1305aes_auto__
#define poly1305aes_auto__

#if defined(POLY_USE_53) || ((!defined(POLY_USE_ppro) && !defined(POLY_USE_athlon)) && (defined(__x86_64__) || defined(_WIN64)))

#ifdef __cplusplus
extern "C" {
#endif

# include "poly1305aes-20050218/aes_big.h"
# include "poly1305aes-20050218/poly1305_53.h"
# include "poly1305aes-20050218/poly1305aes_53.h"

#ifdef __cplusplus
}
#endif

# define poly1305aes_auto_aes_c                       "poly1305aes-20050218/aes_big.c"
# define poly1305aes_auto_aes_s                       "empty.c"
# define poly1305aes_auto_poly1305_c                  "poly1305aes-20050218/poly1305_53.c"
# define poly1305aes_auto_poly1305_s                  "empty.c"
# define poly1305aes_auto_aes_constants_c             "poly1305aes-20050218/aes_big_constants.c"
# define poly1305aes_auto_aes_constants_s             "empty.c"
# define poly1305aes_auto_poly1305aes_clamp_c         "poly1305aes-20050218/poly1305aes_53_clamp.c"
# define poly1305aes_auto_poly1305aes_clamp_s         "empty.c"
# define poly1305aes_auto_poly1305aes_verify_c        "poly1305aes-20050218/poly1305aes_53_verify.c"
# define poly1305aes_auto_poly1305aes_verify_s        "empty.c"
# define poly1305aes_auto_poly1305_constants_c        "poly1305aes-20050218/poly1305_53_constants.c"
# define poly1305aes_auto_poly1305_constants_s        "empty.c"
# define poly1305aes_auto_poly1305aes_isequal_c       "poly1305aes-20050218/poly1305aes_53_isequal.c"
# define poly1305aes_auto_poly1305aes_isequal_s       "empty.c"
# define poly1305aes_auto_poly1305aes_authenticate_c  "poly1305aes-20050218/poly1305aes_53_authenticate.c"
# define poly1305aes_auto_poly1305aes_authenticate_s  "empty.c"

#elif defined(POLY_USE_athlon)

# include "poly1305aes-20050218/aes_athlon.h"
# include "poly1305aes-20050218/poly1305_athlon.h"
# include "poly1305aes-20050218/poly1305aes_athlon.h"

# define poly1305aes_auto_aes_c                      "empty.c"
# define poly1305aes_auto_aes_s                      "poly1305aes-20050218/aes_athlon.s"
# define poly1305aes_auto_poly1305_c                 "empty.c"
# define poly1305aes_auto_poly1305_s                 "poly1305aes-20050218/poly1305_athlon.s"
# define poly1305aes_auto_aes_constants_c            "empty.c"
# define poly1305aes_auto_aes_constants_s            "poly1305aes-20050218/aes_athlon_constants.s"
# define poly1305aes_auto_poly1305aes_clamp_s        "empty.c"
# define poly1305aes_auto_poly1305aes_clamp_c        "poly1305aes-20050218/poly1305aes_athlon_clamp.c"
# define poly1305aes_auto_poly1305aes_verify_s       "empty.c"
# define poly1305aes_auto_poly1305aes_verify_c       "poly1305aes-20050218/poly1305aes_athlon_verify.c"
# define poly1305aes_auto_poly1305_constants_c       "empty.c"
# define poly1305aes_auto_poly1305_constants_s       "poly1305aes-20050218/poly1305_athlon_constants.s"
# define poly1305aes_auto_poly1305aes_isequal_c      "empty.c"
# define poly1305aes_auto_poly1305aes_isequal_s      "poly1305aes-20050218/poly1305aes_athlon_isequal.s"
# define poly1305aes_auto_poly1305aes_authenticate_c "poly1305aes-20050218/poly1305aes_athlon_authenticate.c"
# define poly1305aes_auto_poly1305aes_authenticate_s "empty.c"

#elif defined(POLY_USE_ppro) || ((!defined(POLY_USE_53) && !defined(POLY_USE_athlon)) && (defined(__i386__) || defined(_WIN32)))

# include "poly1305aes-20050218/aes_ppro.h"
# include "poly1305aes-20050218/poly1305_ppro.h"
# include "poly1305aes-20050218/poly1305aes_ppro.h"

# define poly1305aes_auto_aes_c                      "empty.c"
# define poly1305aes_auto_aes_s                      "poly1305aes-20050218/aes_ppro.s"
# define poly1305aes_auto_poly1305_c                 "empty.c"
# define poly1305aes_auto_poly1305_s                 "poly1305aes-20050218/poly1305_ppro.s"
# define poly1305aes_auto_aes_constants_c            "empty.c"
# define poly1305aes_auto_aes_constants_s            "poly1305aes-20050218/aes_ppro_constants.s"
# define poly1305aes_auto_poly1305aes_clamp_s        "empty.c"
# define poly1305aes_auto_poly1305aes_clamp_c        "poly1305aes-20050218/poly1305aes_ppro_clamp.c"
# define poly1305aes_auto_poly1305aes_verify_s       "empty.c"
# define poly1305aes_auto_poly1305aes_verify_c       "poly1305aes-20050218/poly1305aes_ppro_verify.c"
# define poly1305aes_auto_poly1305_constants_c       "empty.c"
# define poly1305aes_auto_poly1305_constants_s       "poly1305aes-20050218/poly1305_ppro_constants.s"
# define poly1305aes_auto_poly1305aes_isequal_c      "empty.c"
# define poly1305aes_auto_poly1305aes_isequal_s      "poly1305aes-20050218/poly1305aes_ppro_isequal.s"
# define poly1305aes_auto_poly1305aes_authenticate_c "poly1305aes-20050218/poly1305aes_ppro_authenticate.c"
# define poly1305aes_auto_poly1305aes_authenticate_s "empty.c"

#endif

#endif
