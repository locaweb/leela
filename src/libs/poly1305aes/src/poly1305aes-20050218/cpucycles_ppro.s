# cpucycles_ppro.s version 20050213
# D. J. Bernstein
# Public domain.

.text
.p2align 4,0x90
.globl cpucycles_ppro
.globl _cpucycles_ppro
cpucycles_ppro:
_cpucycles_ppro:
.byte 15
.byte 49
ret
