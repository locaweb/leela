# cpucycles_sparc.s version 20050131
# D. J. Bernstein
# Public domain.

.section ".text" 
.align 32
.global cpucycles_sparc
cpucycles_sparc:
retl
rd %tick,%o0
