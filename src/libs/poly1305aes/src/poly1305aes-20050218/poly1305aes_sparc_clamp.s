# poly1305aes_sparc_clamp.s version 20050131
# D. J. Bernstein
# Public domain.

# translated by qhasm-sparc version 20050131

# input line 1: register int64 kr

# input line 2: register int64 r3

# input line 3: register int64 r7

# input line 4: register int64 r11

# input line 5: register int64 r15

# input line 6: register int64 r4

# input line 7: register int64 r8

# input line 8: register int64 r12

# input line 9: 

# input line 10: enter poly1305aes_sparc_clamp
.section ".text"
.align 32
.global poly1305aes_sparc_clamp
poly1305aes_sparc_clamp:
save %sp,-176,%sp

# input line 11: input kr

# input line 12: 

# input line 13: r3 = *(uchar *) (kr + 19)
# r3!%l0 = *(uchar *) (kr!%i0 + 19)
ldub [%i0+19],%l0
# live registers: 2 int64, 0 double

# input line 14: r7 = *(uchar *) (kr + 23)
# r7!%l1 = *(uchar *) (kr!%i0 + 23)
ldub [%i0+23],%l1
# live registers: 3 int64, 0 double

# input line 15: r11 = *(uchar *) (kr + 27)
# r11!%l2 = *(uchar *) (kr!%i0 + 27)
ldub [%i0+27],%l2
# live registers: 4 int64, 0 double

# input line 16: r15 = *(uchar *) (kr + 31)
# r15!%l3 = *(uchar *) (kr!%i0 + 31)
ldub [%i0+31],%l3
# live registers: 5 int64, 0 double

# input line 17: r4 = *(uchar *) (kr + 20)
# r4!%l4 = *(uchar *) (kr!%i0 + 20)
ldub [%i0+20],%l4
# live registers: 6 int64, 0 double

# input line 18: r8 = *(uchar *) (kr + 24)
# r8!%l5 = *(uchar *) (kr!%i0 + 24)
ldub [%i0+24],%l5
# live registers: 7 int64, 0 double

# input line 19: r12 = *(uchar *) (kr + 28)
# r12!%l6 = *(uchar *) (kr!%i0 + 28)
ldub [%i0+28],%l6
# live registers: 8 int64, 0 double

# input line 20: 

# input line 21: r3 &= 15
# r3#2!%l0 = r3!%l0 & 15
and %l0,15,%l0
# live registers: 8 int64, 0 double

# input line 22: *(uchar *) (kr + 19) = r3
# *(uchar *) (kr!%i0 + 19) = r3#2!%l0
stub %l0,[%i0+19]
# live registers: 7 int64, 0 double

# input line 23: r7 &= 15
# r7#2!%l0 = r7!%l1 & 15
and %l1,15,%l0
# live registers: 7 int64, 0 double

# input line 24: *(uchar *) (kr + 23) = r7
# *(uchar *) (kr!%i0 + 23) = r7#2!%l0
stub %l0,[%i0+23]
# live registers: 6 int64, 0 double

# input line 25: r11 &= 15
# r11#2!%l0 = r11!%l2 & 15
and %l2,15,%l0
# live registers: 6 int64, 0 double

# input line 26: *(uchar *) (kr + 27) = r11
# *(uchar *) (kr!%i0 + 27) = r11#2!%l0
stub %l0,[%i0+27]
# live registers: 5 int64, 0 double

# input line 27: r15 &= 15
# r15#2!%l0 = r15!%l3 & 15
and %l3,15,%l0
# live registers: 5 int64, 0 double

# input line 28: *(uchar *) (kr + 31) = r15
# *(uchar *) (kr!%i0 + 31) = r15#2!%l0
stub %l0,[%i0+31]
# live registers: 4 int64, 0 double

# input line 29: r4 &= 252
# r4#2!%l0 = r4!%l4 & 252
and %l4,252,%l0
# live registers: 4 int64, 0 double

# input line 30: *(uchar *) (kr + 20) = r4
# *(uchar *) (kr!%i0 + 20) = r4#2!%l0
stub %l0,[%i0+20]
# live registers: 3 int64, 0 double

# input line 31: r8 &= 252
# r8#2!%l0 = r8!%l5 & 252
and %l5,252,%l0
# live registers: 3 int64, 0 double

# input line 32: *(uchar *) (kr + 24) = r8
# *(uchar *) (kr!%i0 + 24) = r8#2!%l0
stub %l0,[%i0+24]
# live registers: 2 int64, 0 double

# input line 33: r12 &= 252
# r12#2!%l0 = r12!%l6 & 252
and %l6,252,%l0
# live registers: 2 int64, 0 double

# input line 34: *(uchar *) (kr + 28) = r12
# *(uchar *) (kr!%i0 + 28) = r12#2!%l0
stub %l0,[%i0+28]
# live registers: 0 int64, 0 double

# input line 35: 

# input line 36: leave
ret
restore
