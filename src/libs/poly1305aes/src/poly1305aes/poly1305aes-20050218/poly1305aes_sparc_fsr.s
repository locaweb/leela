# poly1305aes_sparc_fsr.s version 20050131
# D. J. Bernstein
# Public domain.

.section ".data"
.align 4
.global zero
zero:
.long 0

.section ".text"
.align 32
.global poly1305aes_sparc_fsr
poly1305aes_sparc_fsr:
sethi %hh(zero),%o0
sethi %lm(zero),%o1
or %o0,%hm(zero),%o0
or %o1,%lo(zero),%o1
sllx %o0,32,%o0
retl
ld [%o0+%o1],%fsr
