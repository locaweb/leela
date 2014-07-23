# cpucycles_athlon.s version 20050218
# D. J. Bernstein
# Public domain.

.text
.p2align 4,0x90
.globl cpucycles_athlon
.globl _cpucycles_athlon
cpucycles_athlon:
_cpucycles_athlon:
.byte 15
.byte 49
ret
