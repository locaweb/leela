# cpucycles_macos.s version 20050207
# D. J. Bernstein
# Public domain.

# Is there a reliable way to determine the time base as CPU cycles?
# It's 16 on a G4 I've tested; this code assumes 16 in general.

.text
.align 2
.globl _cpucycles_macos
.globl cpucycles_macos
_cpucycles_macos:
cpucycles_macos:
mftbu r3
mftb r4
mftbu r5
cmpw r3,r5
bne cpucycles_macos
rlwinm r3,r3,4,0xfffffff0
rlwimi r3,r4,4,0x0000000f
rlwinm r4,r4,4,0xfffffff0
blr
