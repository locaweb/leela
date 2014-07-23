# poly1305_aix.s version 20050207
# D. J. Bernstein
# Public domain.

.toc
.addr.poly1305_aix_constants:
.tc poly1305_aix_constants[tc],poly1305_aix_constants[rw]
.extern poly1305_aix_constants[rw]

# input line 1: register int32 out

# input line 2: register int32 r

# input line 3: register int32 s

# input line 4: register int32 m

# input line 5: register int32 l

# input line 6: register int32 constants

# input line 7: temporary mem64 d0

# input line 8: temporary mem64 d1

# input line 9: temporary mem64 d2

# input line 10: temporary mem64 d3

# input line 11: register double scale

# input line 12: register double alpha0

# input line 13: register double alpha32

# input line 14: register double alpha64

# input line 15: register double alpha96

# input line 16: register double alpha130

# input line 17: register double h0

# input line 18: register double h1

# input line 19: register double h2

# input line 20: register double h3

# input line 21: register double h4

# input line 22: register double h5

# input line 23: register double h6

# input line 24: register double h7

# input line 25: register double y7

# input line 26: register double y6

# input line 27: register double y1

# input line 28: register double y0

# input line 29: register double y5

# input line 30: register double y4

# input line 31: register double x7

# input line 32: register double x6

# input line 33: register double x1

# input line 34: register double x0

# input line 35: register double y3

# input line 36: register double y2

# input line 37: register double r3low

# input line 38: register double r0low

# input line 39: register double r3high

# input line 40: register double r0high

# input line 41: register double sr1low

# input line 42: register double x5

# input line 43: register double r3lowx0

# input line 44: register double sr1high

# input line 45: register double x4

# input line 46: register double r1low

# input line 47: register double x3

# input line 48: register double r3highx0

# input line 49: register double r1high

# input line 50: register double x2

# input line 51: register double sr2low

# input line 52: register double r0lowx0

# input line 53: register double sr2high

# input line 54: register double r2low

# input line 55: register double r0highx0

# input line 56: register double r2high

# input line 57: register double sr3low

# input line 58: register double r1lowx0

# input line 59: register double sr3high

# input line 60: register double r1highx0

# input line 61: register double r2lowx0

# input line 62: register double r2highx0

# input line 63: register double z0

# input line 64: register double z1

# input line 65: register double z2

# input line 66: register double z3

# input line 67: register int32 r0

# input line 68: register int32 r1

# input line 69: register int32 r2

# input line 70: register int32 r3

# input line 71: register int32 m0

# input line 72: register int32 m1

# input line 73: register int32 m2

# input line 74: register int32 m3

# input line 75: register int32 m00

# input line 76: register int32 m01

# input line 77: register int32 m02

# input line 78: register int32 m03

# input line 79: register int32 m10

# input line 80: register int32 m11

# input line 81: register int32 m12

# input line 82: register int32 m13

# input line 83: register int32 m20

# input line 84: register int32 m21

# input line 85: register int32 m22

# input line 86: register int32 m23

# input line 87: register int32 m30

# input line 88: register int32 m31

# input line 89: register int32 m32

# input line 90: register int32 m33

# input line 91: register int32 lbelow2

# input line 92: register int32 lbelow3

# input line 93: register int32 lbelow4

# input line 94: register int32 lbelow5

# input line 95: register int32 lbelow6

# input line 96: register int32 lbelow7

# input line 97: register int32 lbelow8

# input line 98: register int32 lbelow9

# input line 99: register int32 lbelow10

# input line 100: register int32 lbelow11

# input line 101: register int32 lbelow12

# input line 102: register int32 lbelow13

# input line 103: register int32 lbelow14

# input line 104: register int32 lbelow15

# input line 105: register double alpham80

# input line 106: register double alpham48

# input line 107: register double alpham16

# input line 108: register double alpha18

# input line 109: register double alpha50

# input line 110: register double alpha82

# input line 111: register double alpha112

# input line 112: register double offset0

# input line 113: register double offset1

# input line 114: register double offset2

# input line 115: register double offset3

# input line 116: register int32 s0

# input line 117: register int32 s1

# input line 118: register int32 s2

# input line 119: register int32 s3

# input line 120: register int32 f

# input line 121: register int32 f0

# input line 122: register int32 f1

# input line 123: register int32 f2

# input line 124: register int32 f3

# input line 125: register int32 f4

# input line 126: register int32 g0

# input line 127: register int32 g1

# input line 128: register int32 g2

# input line 129: register int32 g3

# input line 130: register int32 g4

# input line 131: 

# input line 132: extern poly1305_aix_constants

# input line 133: 

# input line 134: enter poly1305_aix
.csect poly1305_aix[DS]
.globl poly1305_aix
poly1305_aix:
.long .poly1305_aix
.long TOC[tc0]
.long 0
.csect .text[PR]
.globl .poly1305_aix
.poly1305_aix:
stwu 1,-400(1)

# input line 135: input out

# input line 136: input r

# input line 137: input s

# input line 138: input m

# input line 139: input l

# input line 140: 

# input line 141:   round to nearest
mtfsfi 7,0
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 24
# live double values: 18
# live flags values: 0

# input line 142: 

# input line 143:   store callerint 31
# %caller_r31@stack = %caller_r31
# mem32#9 = int32#29
# 384(1) = 31
stw 31,384(1)
# live mem32 values: 1
# live flag values: 0
# live mem64 values: 0
# live int32 values: 23
# live double values: 18
# live flags values: 0

# input line 144:   store callerint 30
# %caller_r30@stack = %caller_r30
# mem32#8 = int32#28
# 380(1) = 30
stw 30,380(1)
# live mem32 values: 2
# live flag values: 0
# live mem64 values: 0
# live int32 values: 22
# live double values: 18
# live flags values: 0

# input line 145:   store callerint 29
# %caller_r29@stack = %caller_r29
# mem32#7 = int32#27
# 376(1) = 29
stw 29,376(1)
# live mem32 values: 3
# live flag values: 0
# live mem64 values: 0
# live int32 values: 21
# live double values: 18
# live flags values: 0

# input line 146:   store callerint 28
# %caller_r28@stack = %caller_r28
# mem32#6 = int32#26
# 372(1) = 28
stw 28,372(1)
# live mem32 values: 4
# live flag values: 0
# live mem64 values: 0
# live int32 values: 20
# live double values: 18
# live flags values: 0

# input line 147:   store callerint 27
# %caller_r27@stack = %caller_r27
# mem32#5 = int32#25
# 368(1) = 27
stw 27,368(1)
# live mem32 values: 5
# live flag values: 0
# live mem64 values: 0
# live int32 values: 19
# live double values: 18
# live flags values: 0

# input line 148:   store callerint 26
# %caller_r26@stack = %caller_r26
# mem32#4 = int32#24
# 364(1) = 26
stw 26,364(1)
# live mem32 values: 6
# live flag values: 0
# live mem64 values: 0
# live int32 values: 18
# live double values: 18
# live flags values: 0

# input line 149:   store callerint 25
# %caller_r25@stack = %caller_r25
# mem32#3 = int32#23
# 360(1) = 25
stw 25,360(1)
# live mem32 values: 7
# live flag values: 0
# live mem64 values: 0
# live int32 values: 17
# live double values: 18
# live flags values: 0

# input line 150:   store callerint 24
# %caller_r24@stack = %caller_r24
# mem32#2 = int32#22
# 356(1) = 24
stw 24,356(1)
# live mem32 values: 8
# live flag values: 0
# live mem64 values: 0
# live int32 values: 16
# live double values: 18
# live flags values: 0

# input line 151:   store callerint 23
# %caller_r23@stack = %caller_r23
# mem32#1 = int32#21
# 352(1) = 23
stw 23,352(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 0
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 152:   store callerfloat 31
# %caller_f31@stack = %caller_f31
# mem64#18 = double#29
# 264(1) = 28
stfd 28,264(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 1
# live int32 values: 15
# live double values: 17
# live flags values: 0

# input line 153:   store callerfloat 30
# %caller_f30@stack = %caller_f30
# mem64#17 = double#28
# 256(1) = 27
stfd 27,256(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 2
# live int32 values: 15
# live double values: 16
# live flags values: 0

# input line 154:   store callerfloat 29
# %caller_f29@stack = %caller_f29
# mem64#16 = double#27
# 248(1) = 26
stfd 26,248(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 3
# live int32 values: 15
# live double values: 15
# live flags values: 0

# input line 155:   store callerfloat 28
# %caller_f28@stack = %caller_f28
# mem64#15 = double#26
# 240(1) = 25
stfd 25,240(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 4
# live int32 values: 15
# live double values: 14
# live flags values: 0

# input line 156:   store callerfloat 27
# %caller_f27@stack = %caller_f27
# mem64#14 = double#25
# 232(1) = 24
stfd 24,232(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 5
# live int32 values: 15
# live double values: 13
# live flags values: 0

# input line 157:   store callerfloat 26
# %caller_f26@stack = %caller_f26
# mem64#13 = double#24
# 224(1) = 23
stfd 23,224(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 6
# live int32 values: 15
# live double values: 12
# live flags values: 0

# input line 158:   store callerfloat 25
# %caller_f25@stack = %caller_f25
# mem64#12 = double#23
# 216(1) = 22
stfd 22,216(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 7
# live int32 values: 15
# live double values: 11
# live flags values: 0

# input line 159:   store callerfloat 24
# %caller_f24@stack = %caller_f24
# mem64#11 = double#22
# 208(1) = 21
stfd 21,208(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 8
# live int32 values: 15
# live double values: 10
# live flags values: 0

# input line 160:   store callerfloat 23
# %caller_f23@stack = %caller_f23
# mem64#10 = double#21
# 200(1) = 20
stfd 20,200(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 9
# live int32 values: 15
# live double values: 9
# live flags values: 0

# input line 161:   store callerfloat 22
# %caller_f22@stack = %caller_f22
# mem64#9 = double#20
# 192(1) = 19
stfd 19,192(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 10
# live int32 values: 15
# live double values: 8
# live flags values: 0

# input line 162:   store callerfloat 21
# %caller_f21@stack = %caller_f21
# mem64#8 = double#19
# 184(1) = 18
stfd 18,184(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 11
# live int32 values: 15
# live double values: 7
# live flags values: 0

# input line 163:   store callerfloat 20
# %caller_f20@stack = %caller_f20
# mem64#7 = double#18
# 176(1) = 17
stfd 17,176(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 12
# live int32 values: 15
# live double values: 6
# live flags values: 0

# input line 164:   store callerfloat 19
# %caller_f19@stack = %caller_f19
# mem64#6 = double#17
# 168(1) = 16
stfd 16,168(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 13
# live int32 values: 15
# live double values: 5
# live flags values: 0

# input line 165:   store callerfloat 18
# %caller_f18@stack = %caller_f18
# mem64#5 = double#16
# 160(1) = 15
stfd 15,160(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 14
# live int32 values: 15
# live double values: 4
# live flags values: 0

# input line 166:   store callerfloat 17
# %caller_f17@stack = %caller_f17
# mem64#4 = double#15
# 152(1) = 14
stfd 14,152(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 15
# live int32 values: 15
# live double values: 3
# live flags values: 0

# input line 167:   store callerfloat 16
# %caller_f16@stack = %caller_f16
# mem64#3 = double#14
# 144(1) = 13
stfd 13,144(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 16
# live int32 values: 15
# live double values: 2
# live flags values: 0

# input line 168:   store callerfloat 15
# %caller_f15@stack = %caller_f15
# mem64#2 = double#13
# 136(1) = 12
stfd 12,136(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 17
# live int32 values: 15
# live double values: 1
# live flags values: 0

# input line 169:   store callerfloat 14
# %caller_f14@stack = %caller_f14
# mem64#1 = double#12
# 128(1) = 11
stfd 11,128(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 0
# live flags values: 0

# input line 170: 

# input line 171:   constants = &poly1305_aix_constants
# constants = poly1305_aix_constants
# int32#6 = poly1305_aix_constants
# 8 = poly1305_aix_constants
lwz 8,.addr.poly1305_aix_constants(2)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 172: 

# input line 173:   r0 = reverse *(uint32 *) r
# r0 = reverse *(uint32 *) (r + 0)
# int32#7 = reverse *(uint32 *) (int32#2 + 0)
# 9 = reverse *(uint32 *) (4 + 0)
lwbrx 9,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 174:   r += 4
# r#2 = r + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 175:   m0 = 65536 * 0x4338
# m0 = 0x4338
# int32#10 = 0x4338
# 12 = 0x4338
lis 12,0x4338
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 176: 

# input line 177:   r1 = reverse *(uint32 *) r
# r1 = reverse *(uint32 *) (r#2 + 0)
# int32#8 = reverse *(uint32 *) (int32#2 + 0)
# 10 = reverse *(uint32 *) (4 + 0)
lwbrx 10,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 178:   r += 4
# r#3 = r#2 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 179:   m1 = 65536 * 0x4538
# m1 = 0x4538
# int32#21 = 0x4538
# 23 = 0x4538
lis 23,0x4538
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 180: 

# input line 181:   r2 = reverse *(uint32 *) r
# r2 = reverse *(uint32 *) (r#3 + 0)
# int32#9 = reverse *(uint32 *) (int32#2 + 0)
# 11 = reverse *(uint32 *) (4 + 0)
lwbrx 11,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 182:   r += 4
# r#4 = r#3 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 183:   m2 = 65536 * 0x4738
# m2 = 0x4738
# int32#22 = 0x4738
# 24 = 0x4738
lis 24,0x4738
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 184: 

# input line 185:   r3 = reverse *(uint32 *) r
# r3 = reverse *(uint32 *) (r#4 + 0)
# int32#2 = reverse *(uint32 *) (int32#2 + 0)
# 4 = reverse *(uint32 *) (4 + 0)
lwbrx 4,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 186:   m3 = 65536 * 0x4938
# m3 = 0x4938
# int32#23 = 0x4938
# 25 = 0x4938
lis 25,0x4938
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 23
# live double values: 0
# live flags values: 0

# input line 187: 

# input line 188:   d0 top = m0
# d0 top = m0
# mem64#19 top = int32#10
# 272(1) top = 12
stw 12,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 189:   inplace d0 bottom = r0
# d0 bottom = r0
# mem64#19 bottom = int32#7
# 272(1) bottom = 9
stw 9,276(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 190: 

# input line 191:   d1 top = m1
# d1 top = m1
# mem64#20 top = int32#21
# 280(1) top = 23
stw 23,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 192:   inplace d1 bottom = r1
# d1 bottom = r1
# mem64#20 bottom = int32#8
# 280(1) bottom = 10
stw 10,284(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 193: 

# input line 194:   d2 top = m2
# d2 top = m2
# mem64#21 top = int32#22
# 288(1) top = 24
stw 24,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 195:   inplace d2 bottom = r2
# d2 bottom = r2
# mem64#21 bottom = int32#9
# 288(1) bottom = 11
stw 11,292(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 196: 

# input line 197:   d3 top = m3
# d3 top = m3
# mem64#22 top = int32#23
# 296(1) top = 25
stw 25,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 198:   inplace d3 bottom = r3
# d3 bottom = r3
# mem64#22 bottom = int32#2
# 296(1) bottom = 4
stw 4,300(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 0
# live flags values: 0

# input line 199: 

# input line 200:   alpha32 = *(double *) (constants + 40)
# alpha32 = *(double *) (constants + 40)
# double#3 = *(double *) (int32#6 + 40)
# 2 = *(double *) (8 + 40)
lfd 2,40(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 1
# live flags values: 0

# input line 201: 

# input line 202:   h0 = alpha32 - alpha32
# h0 = alpha32 - alpha32
# double#7 = double#3 - double#3
# 6 = 2 - 2
fsub 6,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 2
# live flags values: 0

# input line 203: 

# input line 204:   h1 = alpha32 - alpha32
# h1 = alpha32 - alpha32
# double#8 = double#3 - double#3
# 7 = 2 - 2
fsub 7,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 3
# live flags values: 0

# input line 205: 

# input line 206:   alpha0 = *(double *) (constants + 24)
# alpha0 = *(double *) (constants + 24)
# double#2 = *(double *) (int32#6 + 24)
# 1 = *(double *) (8 + 24)
lfd 1,24(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 4
# live flags values: 0

# input line 207:   h2 = alpha32 - alpha32
# h2 = alpha32 - alpha32
# double#9 = double#3 - double#3
# 8 = 2 - 2
fsub 8,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 5
# live flags values: 0

# input line 208: 

# input line 209:   alpha64 = *(double *) (constants + 56)
# alpha64 = *(double *) (constants + 56)
# double#4 = *(double *) (int32#6 + 56)
# 3 = *(double *) (8 + 56)
lfd 3,56(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 6
# live flags values: 0

# input line 210:   h3 = alpha32 - alpha32
# h3 = alpha32 - alpha32
# double#10 = double#3 - double#3
# 9 = 2 - 2
fsub 9,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 7
# live flags values: 0

# input line 211: 

# input line 212:   alpha18 = *(double *) (constants + 32)
# alpha18 = *(double *) (constants + 32)
# double#25 = *(double *) (int32#6 + 32)
# 24 = *(double *) (8 + 32)
lfd 24,32(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 8
# live flags values: 0

# input line 213:   h4 = alpha32 - alpha32
# h4 = alpha32 - alpha32
# double#11 = double#3 - double#3
# 10 = 2 - 2
fsub 10,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 9
# live flags values: 0

# input line 214: 

# input line 215:   r0low = d0
# r0low = d0
# double#1 = mem64#19
# 0 = 272(1)
lfd 0,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 15
# live double values: 10
# live flags values: 0

# input line 216:   h5 = alpha32 - alpha32
# h5 = alpha32 - alpha32
# double#12 = double#3 - double#3
# 11 = 2 - 2
fsub 11,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 15
# live double values: 11
# live flags values: 0

# input line 217: 

# input line 218:   r1low = d1
# r1low = d1
# double#5 = mem64#20
# 4 = 280(1)
lfd 4,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 15
# live double values: 12
# live flags values: 0

# input line 219:   h6 = alpha32 - alpha32
# h6 = alpha32 - alpha32
# double#13 = double#3 - double#3
# 12 = 2 - 2
fsub 12,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 15
# live double values: 13
# live flags values: 0

# input line 220: 

# input line 221:   r2low = d2
# r2low = d2
# double#6 = mem64#21
# 5 = 288(1)
lfd 5,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 14
# live flags values: 0

# input line 222:   h7 = alpha32 - alpha32
# h7 = alpha32 - alpha32
# double#14 = double#3 - double#3
# 13 = 2 - 2
fsub 13,2,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 15
# live flags values: 0

# input line 223: 

# input line 224:   alpha50 = *(double *) (constants + 48)
# alpha50 = *(double *) (constants + 48)
# double#26 = *(double *) (int32#6 + 48)
# 25 = *(double *) (8 + 48)
lfd 25,48(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 16
# live flags values: 0

# input line 225:   r0low -= alpha0
# r0low#2 = r0low - alpha0
# double#16 = double#1 - double#2
# 15 = 0 - 1
fsub 15,0,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 16
# live flags values: 0

# input line 226: 

# input line 227:   alpha82 = *(double *) (constants + 64)
# alpha82 = *(double *) (constants + 64)
# double#28 = *(double *) (int32#6 + 64)
# 27 = *(double *) (8 + 64)
lfd 27,64(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 17
# live flags values: 0

# input line 228:   r1low -= alpha32
# r1low#2 = r1low - alpha32
# double#19 = double#5 - double#3
# 18 = 4 - 2
fsub 18,4,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 17
# live flags values: 0

# input line 229: 

# input line 230:   scale = *(double *) (constants + 96)
# scale = *(double *) (constants + 96)
# double#1 = *(double *) (int32#6 + 96)
# 0 = *(double *) (8 + 96)
lfd 0,96(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 231:   r2low -= alpha64
# r2low#2 = r2low - alpha64
# double#21 = double#6 - double#4
# 20 = 5 - 3
fsub 20,5,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 232: 

# input line 233:   alpha96 = *(double *) (constants + 72)
# alpha96 = *(double *) (constants + 72)
# double#5 = *(double *) (int32#6 + 72)
# 4 = *(double *) (8 + 72)
lfd 4,72(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 19
# live flags values: 0

# input line 234:   r0high = r0low + alpha18
# r0high = r0low#2 + alpha18
# double#15 = double#16 + double#25
# 14 = 15 + 24
fadd 14,15,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 20
# live flags values: 0

# input line 235: 

# input line 236:   r3low = d3
# r3low = d3
# double#6 = mem64#22
# 5 = 296(1)
lfd 5,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 237: 

# input line 238:   alpham80 = *(double *) (constants + 0)
# alpham80 = *(double *) (constants + 0)
# double#23 = *(double *) (int32#6 + 0)
# 22 = *(double *) (8 + 0)
lfd 22,0(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 239:   r1high = r1low + alpha50
# r1high = r1low#2 + alpha50
# double#18 = double#19 + double#26
# 17 = 18 + 25
fadd 17,18,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 240:   sr1low = scale * r1low
# sr1low = scale * r1low#2
# double#17 = double#1 * double#19
# 16 = 0 * 18
fmul 16,0,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 241: 

# input line 242:   alpham48 = *(double *) (constants + 8)
# alpham48 = *(double *) (constants + 8)
# double#24 = *(double *) (int32#6 + 8)
# 23 = *(double *) (8 + 8)
lfd 23,8(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 243:   r2high = r2low + alpha82
# r2high = r2low#2 + alpha82
# double#22 = double#21 + double#28
# 21 = 20 + 27
fadd 21,20,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 244:   sr2low = scale * r2low
# sr2low = scale * r2low#2
# double#20 = double#1 * double#21
# 19 = 0 * 20
fmul 19,0,20
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 245: 

# input line 246:   r0high -= alpha18
# r0high#2 = r0high - alpha18
# double#27 = double#15 - double#25
# 26 = 14 - 24
fsub 26,14,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 247: lgeflags unsigned l - 16
# flags unsigned l - 16
# flags unsigned int32#5 - 16
# flags unsigned 7 - 16
cmplwi 7,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 248: 

# input line 249:   r3low -= alpha96
# r3low#2 = r3low - alpha96
# double#15 = double#6 - double#5
# 14 = 5 - 4
fsub 14,5,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 250: 

# input line 251:   r1high -= alpha50
# r1high#2 = r1high - alpha50
# double#29 = double#18 - double#26
# 28 = 17 - 25
fsub 28,17,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 252: 

# input line 253:   sr1high = sr1low + alpham80
# sr1high = sr1low + alpham80
# double#18 = double#17 + double#23
# 17 = 16 + 22
fadd 17,16,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 254: 

# input line 255:   alpha112 = *(double *) (constants + 80)
# alpha112 = *(double *) (constants + 80)
# double#26 = *(double *) (int32#6 + 80)
# 25 = *(double *) (8 + 80)
lfd 25,80(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 256:   r0low -= r0high
# r0low#3 = r0low#2 - r0high#2
# double#25 = double#16 - double#27
# 24 = 15 - 26
fsub 24,15,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 257: 

# input line 258:   alpham16 = *(double *) (constants + 16)
# alpham16 = *(double *) (constants + 16)
# double#32 = *(double *) (int32#6 + 16)
# 31 = *(double *) (8 + 16)
lfd 31,16(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 259:   r2high -= alpha82
# r2high#2 = r2high - alpha82
# double#31 = double#22 - double#28
# 30 = 21 - 27
fsub 30,21,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 260:   sr3low = scale * r3low
# sr3low = scale * r3low#2
# double#22 = double#1 * double#15
# 21 = 0 * 14
fmul 21,0,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 261: 

# input line 262:   alpha130 = *(double *) (constants + 88)
# alpha130 = *(double *) (constants + 88)
# double#6 = *(double *) (int32#6 + 88)
# 5 = *(double *) (8 + 88)
lfd 5,88(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 29
# live flags values: 1

# input line 263:   sr2high = sr2low + alpham48
# sr2high = sr2low + alpham48
# double#16 = double#20 + double#24
# 15 = 19 + 23
fadd 15,19,23
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 30
# live flags values: 1

# input line 264: 

# input line 265:   r1low -= r1high
# r1low#3 = r1low#2 - r1high#2
# double#28 = double#19 - double#29
# 27 = 18 - 28
fsub 27,18,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 30
# live flags values: 1

# input line 266: 

# input line 267:   sr1high -= alpham80
# sr1high#2 = sr1high - alpham80
# double#18 = double#18 - double#23
# 17 = 17 - 22
fsub 17,17,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 29
# live flags values: 1

# input line 268:   store sr1high
# sr1high@stack = sr1high#2
# mem64#24 = double#18
# 312(1) = 17
stfd 17,312(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 29
# live flags values: 1

# input line 269: 

# input line 270:   r2low -= r2high
# r2low#3 = r2low#2 - r2high#2
# double#30 = double#21 - double#31
# 29 = 20 - 30
fsub 29,20,30
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 29
# live flags values: 1

# input line 271: 

# input line 272:   sr2high -= alpham48
# sr2high#2 = sr2high - alpham48
# double#19 = double#16 - double#24
# 18 = 15 - 23
fsub 18,15,23
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 273:   store sr2high
# sr2high@stack = sr2high#2
# mem64#26 = double#19
# 328(1) = 18
stfd 18,328(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 274: 

# input line 275:   r3high = r3low + alpha112
# r3high = r3low#2 + alpha112
# double#16 = double#15 + double#26
# 15 = 14 + 25
fadd 15,14,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 15
# live double values: 29
# live flags values: 1

# input line 276: 

# input line 277:   sr1low -= sr1high
# sr1low#2 = sr1low - sr1high#2
# double#17 = double#17 - double#18
# 16 = 16 - 17
fsub 16,16,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 278:   store sr1low
# sr1low@stack = sr1low#2
# mem64#23 = double#17
# 304(1) = 16
stfd 16,304(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 279: 

# input line 280:   sr3high = sr3low + alpham16
# sr3high = sr3low + alpham16
# double#18 = double#22 + double#32
# 17 = 21 + 31
fadd 17,21,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 281: 

# input line 282:   sr2low -= sr2high
# sr2low#2 = sr2low - sr2high#2
# double#17 = double#20 - double#19
# 16 = 19 - 18
fsub 16,19,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 283:   store sr2low
# sr2low@stack = sr2low#2
# mem64#25 = double#17
# 320(1) = 16
stfd 16,320(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 284: 

# input line 285:   r3high -= alpha112
# r3high#2 = r3high - alpha112
# double#26 = double#16 - double#26
# 25 = 15 - 25
fsub 25,15,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 286: 

# input line 287:   sr3high -= alpham16
# sr3high#2 = sr3high - alpham16
# double#16 = double#18 - double#32
# 15 = 17 - 31
fsub 15,17,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 288:   store sr3high
# sr3high@stack = sr3high#2
# mem64#28 = double#16
# 344(1) = 15
stfd 15,344(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 23
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 289: 

# input line 290: 

# input line 291:   r3low -= r3high
# r3low#3 = r3low#2 - r3high#2
# double#24 = double#15 - double#26
# 23 = 14 - 25
fsub 23,14,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 23
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 292: 

# input line 293: 

# input line 294:   sr3low -= sr3high
# sr3low#2 = sr3low - sr3high#2
# double#15 = double#22 - double#16
# 14 = 21 - 15
fsub 14,21,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 23
# live int32 values: 15
# live double values: 23
# live flags values: 1

# input line 295:   store sr3low
# sr3low@stack = sr3low#2
# mem64#27 = double#15
# 336(1) = 14
stfd 14,336(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 22
# live flags values: 1

# input line 296: 

# input line 297: goto addatmost15bytes if <
blt .label.addatmost15bytes

# input line 298: 

# input line 299:   m0 = reverse *(uint32 *) m
# m0#2 = reverse *(uint32 *) (m + 0)
# int32#7 = reverse *(uint32 *) (int32#4 + 0)
# 9 = reverse *(uint32 *) (6 + 0)
lwbrx 9,0,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 22
# live flags values: 0

# input line 300:   m += 4
# m#2 = m + 4
# int32#2 = int32#4 + 4
# 4 = 6 + 4
addi 4,6,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 22
# live flags values: 0

# input line 301:   f0 = 65536 * 0x4338
# f0 = 0x4338
# int32#21 = 0x4338
# 23 = 0x4338
lis 23,0x4338
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 17
# live double values: 22
# live flags values: 0

# input line 302: 

# input line 303:   m1 = reverse *(uint32 *) m
# m1#2 = reverse *(uint32 *) (m#2 + 0)
# int32#8 = reverse *(uint32 *) (int32#2 + 0)
# 10 = reverse *(uint32 *) (4 + 0)
lwbrx 10,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 22
# live flags values: 0

# input line 304:   m += 4
# m#3 = m#2 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 22
# live flags values: 0

# input line 305:   f1 = 65536 * 0x4538
# f1 = 0x4538
# int32#22 = 0x4538
# 24 = 0x4538
lis 24,0x4538
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 22
# live flags values: 0

# input line 306: 

# input line 307:   m2 = reverse *(uint32 *) m
# m2#2 = reverse *(uint32 *) (m#3 + 0)
# int32#9 = reverse *(uint32 *) (int32#2 + 0)
# 11 = reverse *(uint32 *) (4 + 0)
lwbrx 11,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 22
# live flags values: 0

# input line 308:   m += 4
# m#4 = m#3 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 22
# live flags values: 0

# input line 309:   f2 = 65536 * 0x4738
# f2 = 0x4738
# int32#23 = 0x4738
# 25 = 0x4738
lis 25,0x4738
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 22
# live flags values: 0

# input line 310: 

# input line 311:   m3 = reverse *(uint32 *) m
# m3#2 = reverse *(uint32 *) (m#4 + 0)
# int32#10 = reverse *(uint32 *) (int32#2 + 0)
# 12 = reverse *(uint32 *) (4 + 0)
lwbrx 12,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 22
# live flags values: 0

# input line 312:   m += 4
# m = m#4 + 4
# int32#4 = int32#2 + 4
# 6 = 4 + 4
addi 6,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 22
# live flags values: 0

# input line 313:   f3 = 65536 * 0x4938
# f3 = 0x4938
# int32#2 = 0x4938
# 4 = 0x4938
lis 4,0x4938
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 22
# live flags values: 0

# input line 314: 

# input line 315:   l -= 16
# l = l - 16
# int32#5 = int32#5 - 16
# 7 = 7 - 16
addi 7,7,-16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 22
# live flags values: 0

# input line 316:   f3 += 1
# f3#2 = f3 + 1
# int32#2 = int32#2 + 1
# 4 = 4 + 1
addi 4,4,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 22
# live flags values: 0

# input line 317: 

# input line 318:   d0 top = f0
# d0#2 top = f0
# mem64#19 top = int32#21
# 272(1) top = 23
stw 23,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 22
# live double values: 22
# live flags values: 0

# input line 319:   inplace d0 bottom = m0
# d0#2 bottom = m0#2
# mem64#19 bottom = int32#7
# 272(1) bottom = 9
stw 9,276(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 21
# live double values: 22
# live flags values: 0

# input line 320:   d1 top = f1
# d1#2 top = f1
# mem64#20 top = int32#22
# 280(1) top = 24
stw 24,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 20
# live double values: 22
# live flags values: 0

# input line 321:   inplace d1 bottom = m1
# d1#2 bottom = m1#2
# mem64#20 bottom = int32#8
# 280(1) bottom = 10
stw 10,284(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 19
# live double values: 22
# live flags values: 0

# input line 322:   d2 top = f2
# d2#2 top = f2
# mem64#21 top = int32#23
# 288(1) top = 25
stw 25,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 18
# live double values: 22
# live flags values: 0

# input line 323:   inplace d2 bottom = m2
# d2#2 bottom = m2#2
# mem64#21 bottom = int32#9
# 288(1) bottom = 11
stw 11,292(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 17
# live double values: 22
# live flags values: 0

# input line 324:   d3 top = f3
# d3#2 top = f3#2
# mem64#22 top = int32#2
# 296(1) top = 4
stw 4,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 16
# live double values: 22
# live flags values: 0

# input line 325:   inplace d3 bottom = m3
# d3#2 bottom = m3#2
# mem64#22 bottom = int32#10
# 296(1) bottom = 12
stw 12,300(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 326: 

# input line 327: lgeflags unsigned l - 16
# flags unsigned l - 16
# flags unsigned int32#5 - 16
# flags unsigned 7 - 16
cmplwi 7,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 22
# live flags values: 1

# input line 328: 

# input line 329:   z0 = d0
# z0 = d0#2
# double#15 = mem64#19
# 14 = 272(1)
lfd 14,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 15
# live double values: 23
# live flags values: 1

# input line 330: 

# input line 331:   z1 = d1
# z1 = d1#2
# double#16 = mem64#20
# 15 = 280(1)
lfd 15,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 332: 

# input line 333:   z2 = d2
# z2 = d2#2
# double#17 = mem64#21
# 16 = 288(1)
lfd 16,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 334: 

# input line 335:   z3 = d3
# z3 = d3#2
# double#18 = mem64#22
# 17 = 296(1)
lfd 17,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 336: 

# input line 337:   z0 -= alpha0
# z0#2 = z0 - alpha0
# double#2 = double#15 - double#2
# 1 = 14 - 1
fsub 1,14,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 338: 

# input line 339:   z1 -= alpha32
# z1#2 = z1 - alpha32
# double#15 = double#16 - double#3
# 14 = 15 - 2
fsub 14,15,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 340: 

# input line 341:   z2 -= alpha64
# z2#2 = z2 - alpha64
# double#16 = double#17 - double#4
# 15 = 16 - 3
fsub 15,16,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 342: 

# input line 343:   z3 -= alpha96
# z3#2 = z3 - alpha96
# double#17 = double#18 - double#5
# 16 = 17 - 4
fsub 16,17,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 344: 

# input line 345:   h0 += z0
# h0#2 = h0 + z0#2
# double#2 = double#7 + double#2
# 1 = 6 + 1
fadd 1,6,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 346: 

# input line 347:   h1 += z1
# h1#2 = h1 + z1#2
# double#8 = double#8 + double#15
# 7 = 7 + 14
fadd 7,7,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 23
# live flags values: 1

# input line 348: 

# input line 349:   h3 += z2
# h3#2 = h3 + z2#2
# double#10 = double#10 + double#16
# 9 = 9 + 15
fadd 9,9,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 22
# live flags values: 1

# input line 350: 

# input line 351:   h5 += z3
# h5#2 = h5 + z3#2
# double#12 = double#12 + double#17
# 11 = 11 + 16
fadd 11,11,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 21
# live flags values: 1

# input line 352: 

# input line 353: goto multiplyaddatmost15bytes if <
blt .label.multiplyaddatmost15bytes

# input line 354: 

# input line 355: multiplyaddatleast16bytes
.label.multiplyaddatleast16bytes:

# input line 356: 

# input line 357:   m0 = reverse *(uint32 *) m
# m0#3 = reverse *(uint32 *) (m + 0)
# int32#7 = reverse *(uint32 *) (int32#4 + 0)
# 9 = reverse *(uint32 *) (6 + 0)
lwbrx 9,0,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 21
# live flags values: 0

# input line 358:   y1 = h1 + alpha32
# y1 = h1#2 + alpha32
# double#16 = double#8 + double#3
# 15 = 7 + 2
fadd 15,7,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 22
# live flags values: 0

# input line 359: 

# input line 360:   m += 4
# m#5 = m + 4
# int32#2 = int32#4 + 4
# 4 = 6 + 4
addi 4,6,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 22
# live flags values: 0

# input line 361:   y0 = h0 + alpha32
# y0 = h0#2 + alpha32
# double#17 = double#2 + double#3
# 16 = 1 + 2
fadd 16,1,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 23
# live flags values: 0

# input line 362: 

# input line 363:   f0 = 65536 * 0x4338
# f0#2 = 0x4338
# int32#21 = 0x4338
# 23 = 0x4338
lis 23,0x4338
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 17
# live double values: 23
# live flags values: 0

# input line 364:   y7 = h7 + alpha130
# y7 = h7 + alpha130
# double#7 = double#14 + double#6
# 6 = 13 + 5
fadd 6,13,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 17
# live double values: 24
# live flags values: 0

# input line 365: 

# input line 366:   m1 = reverse *(uint32 *) m
# m1#3 = reverse *(uint32 *) (m#5 + 0)
# int32#8 = reverse *(uint32 *) (int32#2 + 0)
# 10 = reverse *(uint32 *) (4 + 0)
lwbrx 10,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 24
# live flags values: 0

# input line 367:   y6 = h6 + alpha130
# y6 = h6 + alpha130
# double#15 = double#13 + double#6
# 14 = 12 + 5
fadd 14,12,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 25
# live flags values: 0

# input line 368: 

# input line 369:   m += 4
# m#6 = m#5 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 25
# live flags values: 0

# input line 370:   y5 = h5 + alpha96
# y5 = h5#2 + alpha96
# double#18 = double#12 + double#5
# 17 = 11 + 4
fadd 17,11,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 26
# live flags values: 0

# input line 371: 

# input line 372:   f1 = 65536 * 0x4538
# f1#2 = 0x4538
# int32#22 = 0x4538
# 24 = 0x4538
lis 24,0x4538
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 26
# live flags values: 0

# input line 373:   y4 = h4 + alpha96
# y4 = h4 + alpha96
# double#19 = double#11 + double#5
# 18 = 10 + 4
fadd 18,10,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 27
# live flags values: 0

# input line 374: 

# input line 375:   m2 = reverse *(uint32 *) m
# m2#3 = reverse *(uint32 *) (m#6 + 0)
# int32#9 = reverse *(uint32 *) (int32#2 + 0)
# 11 = reverse *(uint32 *) (4 + 0)
lwbrx 11,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 27
# live flags values: 0

# input line 376:   y3 = h3 + alpha64
# y3 = h3#2 + alpha64
# double#20 = double#10 + double#4
# 19 = 9 + 3
fadd 19,9,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 28
# live flags values: 0

# input line 377: 

# input line 378:   m += 4
# m#7 = m#6 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 28
# live flags values: 0

# input line 379:   y2 = h2 + alpha64
# y2 = h2 + alpha64
# double#21 = double#9 + double#4
# 20 = 8 + 3
fadd 20,8,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 29
# live flags values: 0

# input line 380: 

# input line 381:   f2 = 65536 * 0x4738
# f2#2 = 0x4738
# int32#23 = 0x4738
# 25 = 0x4738
lis 25,0x4738
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 29
# live flags values: 0

# input line 382:   y1 -= alpha32
# y1#2 = y1 - alpha32
# double#16 = double#16 - double#3
# 15 = 15 - 2
fsub 15,15,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 29
# live flags values: 0

# input line 383: 

# input line 384:   m3 = reverse *(uint32 *) m
# m3#3 = reverse *(uint32 *) (m#7 + 0)
# int32#10 = reverse *(uint32 *) (int32#2 + 0)
# 12 = reverse *(uint32 *) (4 + 0)
lwbrx 12,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 29
# live flags values: 0

# input line 385:   y0 -= alpha32
# y0#2 = y0 - alpha32
# double#17 = double#17 - double#3
# 16 = 16 - 2
fsub 16,16,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 29
# live flags values: 0

# input line 386: 

# input line 387:   m += 4
# m = m#7 + 4
# int32#4 = int32#2 + 4
# 6 = 4 + 4
addi 6,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 29
# live flags values: 0

# input line 388:   y7 -= alpha130
# y7#2 = y7 - alpha130
# double#7 = double#7 - double#6
# 6 = 6 - 5
fsub 6,6,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 29
# live flags values: 0

# input line 389: 

# input line 390:   f3 = 65536 * 0x4938
# f3#3 = 0x4938
# int32#2 = 0x4938
# 4 = 0x4938
lis 4,0x4938
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 29
# live flags values: 0

# input line 391:   y6 -= alpha130
# y6#2 = y6 - alpha130
# double#15 = double#15 - double#6
# 14 = 14 - 5
fsub 14,14,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 29
# live flags values: 0

# input line 392: 

# input line 393:   l -= 16
# l = l - 16
# int32#5 = int32#5 - 16
# 7 = 7 - 16
addi 7,7,-16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 29
# live flags values: 0

# input line 394:   y5 -= alpha96
# y5#2 = y5 - alpha96
# double#18 = double#18 - double#5
# 17 = 17 - 4
fsub 17,17,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 29
# live flags values: 0

# input line 395: 

# input line 396:   f3 += 1
# f3#4 = f3#3 + 1
# int32#2 = int32#2 + 1
# 4 = 4 + 1
addi 4,4,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 29
# live flags values: 0

# input line 397:   y4 -= alpha96
# y4#2 = y4 - alpha96
# double#19 = double#19 - double#5
# 18 = 18 - 4
fsub 18,18,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 29
# live flags values: 0

# input line 398: 

# input line 399:   d0 top = f0
# d0#3 top = f0#2
# mem64#19 top = int32#21
# 272(1) top = 23
stw 23,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 22
# live double values: 29
# live flags values: 0

# input line 400:   y3 -= alpha64
# y3#2 = y3 - alpha64
# double#22 = double#20 - double#4
# 21 = 19 - 3
fsub 21,19,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 22
# live double values: 29
# live flags values: 0

# input line 401: 

# input line 402:   inplace d0 bottom = m0
# d0#3 bottom = m0#3
# mem64#19 bottom = int32#7
# 272(1) bottom = 9
stw 9,276(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 21
# live double values: 29
# live flags values: 0

# input line 403:   y2 -= alpha64
# y2#2 = y2 - alpha64
# double#23 = double#21 - double#4
# 22 = 20 - 3
fsub 22,20,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 21
# live double values: 29
# live flags values: 0

# input line 404: 

# input line 405:   d1 top = f1
# d1#3 top = f1#2
# mem64#20 top = int32#22
# 280(1) top = 24
stw 24,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 20
# live double values: 29
# live flags values: 0

# input line 406:   x1 = h1 - y1
# x1 = h1#2 - y1#2
# double#20 = double#8 - double#16
# 19 = 7 - 15
fsub 19,7,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 20
# live double values: 29
# live flags values: 0

# input line 407: 

# input line 408:   inplace d1 bottom = m1
# d1#3 bottom = m1#3
# mem64#20 bottom = int32#8
# 280(1) bottom = 10
stw 10,284(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 19
# live double values: 29
# live flags values: 0

# input line 409:   x0 = h0 - y0
# x0 = h0#2 - y0#2
# double#21 = double#2 - double#17
# 20 = 1 - 16
fsub 20,1,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 19
# live double values: 29
# live flags values: 0

# input line 410: 

# input line 411:   d2 top = f2
# d2#3 top = f2#2
# mem64#21 top = int32#23
# 288(1) top = 25
stw 25,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 18
# live double values: 29
# live flags values: 0

# input line 412:   x7 = h7 - y7
# x7 = h7 - y7#2
# double#2 = double#14 - double#7
# 1 = 13 - 6
fsub 1,13,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 18
# live double values: 29
# live flags values: 0

# input line 413: 

# input line 414:   inplace d2 bottom = m2
# d2#3 bottom = m2#3
# mem64#21 bottom = int32#9
# 288(1) bottom = 11
stw 11,292(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 17
# live double values: 29
# live flags values: 0

# input line 415:   x6 = h6 - y6
# x6 = h6 - y6#2
# double#8 = double#13 - double#15
# 7 = 12 - 14
fsub 7,12,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 17
# live double values: 29
# live flags values: 0

# input line 416: 

# input line 417:   d3 top = f3
# d3#3 top = f3#4
# mem64#22 top = int32#2
# 296(1) top = 4
stw 4,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 16
# live double values: 29
# live flags values: 0

# input line 418:   x5 = h5 - y5
# x5 = h5#2 - y5#2
# double#12 = double#12 - double#18
# 11 = 11 - 17
fsub 11,11,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 16
# live double values: 29
# live flags values: 0

# input line 419: 

# input line 420:   inplace d3 bottom = m3
# d3#3 bottom = m3#3
# mem64#22 bottom = int32#10
# 296(1) bottom = 12
stw 12,300(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 421:   x4 = h4 - y4
# x4 = h4 - y4#2
# double#11 = double#11 - double#19
# 10 = 10 - 18
fsub 10,10,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 422: 

# input line 423:   x3 = h3 - y3
# x3 = h3#2 - y3#2
# double#13 = double#10 - double#22
# 12 = 9 - 21
fsub 12,9,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 424: 

# input line 425:   x2 = h2 - y2
# x2 = h2 - y2#2
# double#14 = double#9 - double#23
# 13 = 8 - 22
fsub 13,8,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 426: 

# input line 427:   x1 += y7 * scale
# x1#2 = x1 + y7#2 * scale
# double#9 = double#20 + double#7 * double#1
# 8 = 19 + 6 * 0
fmadd 8,6,0,19
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 28
# live flags values: 0

# input line 428: 

# input line 429:   x0 += y6 * scale
# x0#2 = x0 + y6#2 * scale
# double#10 = double#21 + double#15 * double#1
# 9 = 20 + 14 * 0
fmadd 9,14,0,20
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 430: 

# input line 431:   x7 += y5
# x7#2 = x7 + y5#2
# double#2 = double#2 + double#18
# 1 = 1 + 17
fadd 1,1,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 432: 

# input line 433:   x6 += y4
# x6#2 = x6 + y4#2
# double#7 = double#8 + double#19
# 6 = 7 + 18
fadd 6,7,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 434: 

# input line 435:   x5 += y3
# x5#2 = x5 + y3#2
# double#8 = double#12 + double#22
# 7 = 11 + 21
fadd 7,11,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 436: 

# input line 437:   x4 += y2
# x4#2 = x4 + y2#2
# double#11 = double#11 + double#23
# 10 = 10 + 22
fadd 10,10,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 438: 

# input line 439:   x3 += y1
# x3#2 = x3 + y1#2
# double#12 = double#13 + double#16
# 11 = 12 + 15
fadd 11,12,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 440: 

# input line 441:   x2 += y0
# x2#2 = x2 + y0#2
# double#13 = double#14 + double#17
# 12 = 13 + 16
fadd 12,13,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 442: 

# input line 443:   x0 += x1
# x0#3 = x0#2 + x1#2
# double#14 = double#10 + double#9
# 13 = 9 + 8
fadd 13,9,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 20
# live flags values: 0

# input line 444: 

# input line 445:   x6 += x7
# x6#3 = x6#2 + x7#2
# double#15 = double#7 + double#2
# 14 = 6 + 1
fadd 14,6,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 19
# live flags values: 0

# input line 446: 

# input line 447:   x4 += x5
# x4#3 = x4#2 + x5#2
# double#18 = double#11 + double#8
# 17 = 10 + 7
fadd 17,10,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 448: 

# input line 449:   x2 += x3
# x2#3 = x2#2 + x3#2
# double#16 = double#13 + double#12
# 15 = 12 + 11
fadd 15,12,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 17
# live flags values: 0

# input line 450: 

# input line 451:   h7 = r3high * x0
# h7#2 = r3high#2 * x0#3
# double#13 = double#26 * double#14
# 12 = 25 * 13
fmul 12,25,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 452: 

# input line 453:   h6 = r3low * x0
# h6#2 = r3low#3 * x0#3
# double#12 = double#24 * double#14
# 11 = 23 * 13
fmul 11,23,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 19
# live flags values: 0

# input line 454: 

# input line 455:   h5 = r2high * x0
# h5#3 = r2high#2 * x0#3
# double#11 = double#31 * double#14
# 10 = 30 * 13
fmul 10,30,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 20
# live flags values: 0

# input line 456: 

# input line 457:   h4 = r2low * x0
# h4#2 = r2low#3 * x0#3
# double#10 = double#30 * double#14
# 9 = 29 * 13
fmul 9,29,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 458: 

# input line 459:   h3 = r1high * x0
# h3#3 = r1high#2 * x0#3
# double#9 = double#29 * double#14
# 8 = 28 * 13
fmul 8,28,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 460: 

# input line 461:   h2 = r1low * x0
# h2#2 = r1low#3 * x0#3
# double#8 = double#28 * double#14
# 7 = 27 * 13
fmul 7,27,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 462: 

# input line 463:   h1 = r0high * x0
# h1#3 = r0high#2 * x0#3
# double#7 = double#27 * double#14
# 6 = 26 * 13
fmul 6,26,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 464: 

# input line 465:   h0 = r0low * x0
# h0#3 = r0low#3 * x0#3
# double#2 = double#25 * double#14
# 1 = 24 * 13
fmul 1,24,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 466: lgeflags unsigned l - 16
# flags unsigned l - 16
# flags unsigned int32#5 - 16
# flags unsigned 7 - 16
cmplwi 7,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 467: 

# input line 468:   h7 += r2high * x2
# h7#3 = h7#2 + r2high#2 * x2#3
# double#14 = double#13 + double#31 * double#16
# 13 = 12 + 30 * 15
fmadd 13,30,15,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 469: 

# input line 470:   h6 += r2low * x2
# h6#3 = h6#2 + r2low#3 * x2#3
# double#13 = double#12 + double#30 * double#16
# 12 = 11 + 29 * 15
fmadd 12,29,15,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 471: 

# input line 472:   h5 += r1high * x2
# h5#4 = h5#3 + r1high#2 * x2#3
# double#12 = double#11 + double#29 * double#16
# 11 = 10 + 28 * 15
fmadd 11,28,15,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 473: 

# input line 474:   h4 += r1low * x2
# h4#3 = h4#2 + r1low#3 * x2#3
# double#11 = double#10 + double#28 * double#16
# 10 = 9 + 27 * 15
fmadd 10,27,15,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 475: 

# input line 476:   h3 += r0high * x2
# h3#4 = h3#3 + r0high#2 * x2#3
# double#10 = double#9 + double#27 * double#16
# 9 = 8 + 26 * 15
fmadd 9,26,15,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 477: 

# input line 478:   h2 += r0low * x2
# h2#3 = h2#2 + r0low#3 * x2#3
# double#9 = double#8 + double#25 * double#16
# 8 = 7 + 24 * 15
fmadd 8,24,15,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 479: 

# input line 480:   h7 += r1high * x4
# h7#4 = h7#3 + r1high#2 * x4#3
# double#14 = double#14 + double#29 * double#18
# 13 = 13 + 28 * 17
fmadd 13,28,17,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 481: 

# input line 482:   h6 += r1low * x4
# h6#4 = h6#3 + r1low#3 * x4#3
# double#8 = double#13 + double#28 * double#18
# 7 = 12 + 27 * 17
fmadd 7,27,17,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 483: 

# input line 484:   load sr3high
# sr3high#3 = sr3high@stack
# double#17 = mem64#28
# 16 = 344(1)
lfd 16,344(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 485:   h5 += r0high * x4
# h5#5 = h5#4 + r0high#2 * x4#3
# double#12 = double#12 + double#27 * double#18
# 11 = 11 + 26 * 17
fmadd 11,26,17,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 486: 

# input line 487:   load sr3low
# sr3low#3 = sr3low@stack
# double#21 = mem64#27
# 20 = 336(1)
lfd 20,336(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 488:   h4 += r0low * x4
# h4#4 = h4#3 + r0low#3 * x4#3
# double#11 = double#11 + double#25 * double#18
# 10 = 10 + 24 * 17
fmadd 10,24,17,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 489: 

# input line 490:   h7 += r0high * x6
# h7 = h7#4 + r0high#2 * x6#3
# double#14 = double#14 + double#27 * double#15
# 13 = 13 + 26 * 14
fmadd 13,26,14,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 491: 

# input line 492:   h6 += r0low * x6
# h6 = h6#4 + r0low#3 * x6#3
# double#13 = double#8 + double#25 * double#15
# 12 = 7 + 24 * 14
fmadd 12,24,14,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 493: 

# input line 494:   h1 += sr3high * x2
# h1#4 = h1#3 + sr3high#3 * x2#3
# double#8 = double#7 + double#17 * double#16
# 7 = 6 + 16 * 15
fmadd 7,16,15,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 495: 

# input line 496:   h0 += sr3low * x2
# h0#4 = h0#3 + sr3low#3 * x2#3
# double#7 = double#2 + double#21 * double#16
# 6 = 1 + 20 * 15
fmadd 6,20,15,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 497: 

# input line 498:   load sr2high
# sr2high#3 = sr2high@stack
# double#20 = mem64#26
# 19 = 328(1)
lfd 19,328(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 499:   h3 += sr3high * x4
# h3#5 = h3#4 + sr3high#3 * x4#3
# double#10 = double#10 + double#17 * double#18
# 9 = 9 + 16 * 17
fmadd 9,16,17,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 500: 

# input line 501:   load sr2low
# sr2low#3 = sr2low@stack
# double#19 = mem64#25
# 18 = 320(1)
lfd 18,320(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 502:   h2 += sr3low * x4
# h2#4 = h2#3 + sr3low#3 * x4#3
# double#9 = double#9 + double#21 * double#18
# 8 = 8 + 20 * 17
fmadd 8,20,17,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 503: 

# input line 504:   h5 += sr3high * x6
# h5#6 = h5#5 + sr3high#3 * x6#3
# double#12 = double#12 + double#17 * double#15
# 11 = 11 + 16 * 14
fmadd 11,16,14,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 505: 

# input line 506:   load sr1high
# sr1high#3 = sr1high@stack
# double#17 = mem64#24
# 16 = 312(1)
lfd 16,312(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 507:   h4 += sr3low * x6
# h4 = h4#4 + sr3low#3 * x6#3
# double#11 = double#11 + double#21 * double#15
# 10 = 10 + 20 * 14
fmadd 10,20,14,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 508: 

# input line 509:   load sr1low
# sr1low#3 = sr1low@stack
# double#16 = mem64#23
# 15 = 304(1)
lfd 15,304(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 510:   h1 += sr2high * x4
# h1#5 = h1#4 + sr2high#3 * x4#3
# double#8 = double#8 + double#20 * double#18
# 7 = 7 + 19 * 17
fmadd 7,19,17,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 511: 

# input line 512:   alpha0 = *(double *) (constants + 24)
# alpha0#2 = *(double *) (constants + 24)
# double#2 = *(double *) (int32#6 + 24)
# 1 = *(double *) (8 + 24)
lfd 1,24(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 513:   h0 += sr2low * x4
# h0#5 = h0#4 + sr2low#3 * x4#3
# double#7 = double#7 + double#19 * double#18
# 6 = 6 + 18 * 17
fmadd 6,18,17,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 514: 

# input line 515:   z3 = d3
# z3#3 = d3#3
# double#21 = mem64#22
# 20 = 296(1)
lfd 20,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 516:   h3 += sr2high * x6
# h3#6 = h3#5 + sr2high#3 * x6#3
# double#10 = double#10 + double#20 * double#15
# 9 = 9 + 19 * 14
fmadd 9,19,14,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 517: 

# input line 518:   z2 = d2
# z2#3 = d2#3
# double#20 = mem64#21
# 19 = 288(1)
lfd 19,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 519:   h2 += sr2low * x6
# h2 = h2#4 + sr2low#3 * x6#3
# double#9 = double#9 + double#19 * double#15
# 8 = 8 + 18 * 14
fmadd 8,18,14,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 520: 

# input line 521:   z1 = d1
# z1#3 = d1#3
# double#18 = mem64#20
# 17 = 280(1)
lfd 17,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 522:   h1 += sr1high * x6
# h1#6 = h1#5 + sr1high#3 * x6#3
# double#8 = double#8 + double#17 * double#15
# 7 = 7 + 16 * 14
fmadd 7,16,14,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 15
# live double values: 27
# live flags values: 1

# input line 523: 

# input line 524:   z0 = d0
# z0#3 = d0#3
# double#17 = mem64#19
# 16 = 272(1)
lfd 16,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 28
# live flags values: 1

# input line 525:   h0 += sr1low * x6
# h0#6 = h0#5 + sr1low#3 * x6#3
# double#7 = double#7 + double#16 * double#15
# 6 = 6 + 15 * 14
fmadd 6,15,14,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 526: 

# input line 527:   z3 -= alpha96
# z3#4 = z3#3 - alpha96
# double#19 = double#21 - double#5
# 18 = 20 - 4
fsub 18,20,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 528: 

# input line 529:   z2 -= alpha64
# z2#4 = z2#3 - alpha64
# double#16 = double#20 - double#4
# 15 = 19 - 3
fsub 15,19,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 530: 

# input line 531:   z1 -= alpha32
# z1#4 = z1#3 - alpha32
# double#15 = double#18 - double#3
# 14 = 17 - 2
fsub 14,17,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 1

# input line 532: 

# input line 533:   z0 -= alpha0
# z0#4 = z0#3 - alpha0#2
# double#2 = double#17 - double#2
# 1 = 16 - 1
fsub 1,16,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 1

# input line 534: 

# input line 535:   h5 += z3
# h5#2 = h5#6 + z3#4
# double#12 = double#12 + double#19
# 11 = 11 + 18
fadd 11,11,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 1

# input line 536: 

# input line 537:   h3 += z2
# h3#2 = h3#6 + z2#4
# double#10 = double#10 + double#16
# 9 = 9 + 15
fadd 9,9,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 23
# live flags values: 1

# input line 538: 

# input line 539:   h1 += z1
# h1#2 = h1#6 + z1#4
# double#8 = double#8 + double#15
# 7 = 7 + 14
fadd 7,7,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 22
# live flags values: 1

# input line 540: 

# input line 541:   h0 += z0
# h0#2 = h0#6 + z0#4
# double#2 = double#7 + double#2
# 1 = 6 + 1
fadd 1,6,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 21
# live flags values: 1

# input line 542: 

# input line 543: goto multiplyaddatleast16bytes if >=
bge .label.multiplyaddatleast16bytes

# input line 544: 

# input line 545: multiplyaddatmost15bytes
.label.multiplyaddatmost15bytes:

# input line 546: 

# input line 547:   y1 = h1 + alpha32
# y1#3 = h1#2 + alpha32
# double#16 = double#8 + double#3
# 15 = 7 + 2
fadd 15,7,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 548: 

# input line 549:   y0 = h0 + alpha32
# y0#3 = h0#2 + alpha32
# double#17 = double#2 + double#3
# 16 = 1 + 2
fadd 16,1,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 550: 

# input line 551:   y7 = h7 + alpha130
# y7#3 = h7 + alpha130
# double#7 = double#14 + double#6
# 6 = 13 + 5
fadd 6,13,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 552: 

# input line 553:   y6 = h6 + alpha130
# y6#3 = h6 + alpha130
# double#15 = double#13 + double#6
# 14 = 12 + 5
fadd 14,12,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 554: 

# input line 555:   y5 = h5 + alpha96
# y5#3 = h5#2 + alpha96
# double#18 = double#12 + double#5
# 17 = 11 + 4
fadd 17,11,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 556: 

# input line 557:   y4 = h4 + alpha96
# y4#3 = h4 + alpha96
# double#19 = double#11 + double#5
# 18 = 10 + 4
fadd 18,10,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 558: 

# input line 559:   y3 = h3 + alpha64
# y3#3 = h3#2 + alpha64
# double#20 = double#10 + double#4
# 19 = 9 + 3
fadd 19,9,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 28
# live flags values: 0

# input line 560: 

# input line 561:   y2 = h2 + alpha64
# y2#3 = h2 + alpha64
# double#21 = double#9 + double#4
# 20 = 8 + 3
fadd 20,8,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 562: 

# input line 563:   y1 -= alpha32
# y1#4 = y1#3 - alpha32
# double#16 = double#16 - double#3
# 15 = 15 - 2
fsub 15,15,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 564: 

# input line 565:   y0 -= alpha32
# y0#4 = y0#3 - alpha32
# double#17 = double#17 - double#3
# 16 = 16 - 2
fsub 16,16,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 566: 

# input line 567:   y7 -= alpha130
# y7#4 = y7#3 - alpha130
# double#7 = double#7 - double#6
# 6 = 6 - 5
fsub 6,6,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 568: 

# input line 569:   y6 -= alpha130
# y6#4 = y6#3 - alpha130
# double#15 = double#15 - double#6
# 14 = 14 - 5
fsub 14,14,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 570: 

# input line 571:   y5 -= alpha96
# y5#4 = y5#3 - alpha96
# double#18 = double#18 - double#5
# 17 = 17 - 4
fsub 17,17,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 572: 

# input line 573:   y4 -= alpha96
# y4#4 = y4#3 - alpha96
# double#19 = double#19 - double#5
# 18 = 18 - 4
fsub 18,18,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 574: 

# input line 575:   y3 -= alpha64
# y3#4 = y3#3 - alpha64
# double#22 = double#20 - double#4
# 21 = 19 - 3
fsub 21,19,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 576: 

# input line 577:   y2 -= alpha64
# y2#4 = y2#3 - alpha64
# double#23 = double#21 - double#4
# 22 = 20 - 3
fsub 22,20,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 578: 

# input line 579:   x1 = h1 - y1
# x1#3 = h1#2 - y1#4
# double#20 = double#8 - double#16
# 19 = 7 - 15
fsub 19,7,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 580: 

# input line 581:   x0 = h0 - y0
# x0#4 = h0#2 - y0#4
# double#21 = double#2 - double#17
# 20 = 1 - 16
fsub 20,1,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 582: 

# input line 583:   x7 = h7 - y7
# x7#3 = h7 - y7#4
# double#2 = double#14 - double#7
# 1 = 13 - 6
fsub 1,13,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 584: 

# input line 585:   x6 = h6 - y6
# x6#4 = h6 - y6#4
# double#8 = double#13 - double#15
# 7 = 12 - 14
fsub 7,12,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 586: 

# input line 587:   x5 = h5 - y5
# x5#3 = h5#2 - y5#4
# double#12 = double#12 - double#18
# 11 = 11 - 17
fsub 11,11,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 588: 

# input line 589:   x4 = h4 - y4
# x4#4 = h4 - y4#4
# double#11 = double#11 - double#19
# 10 = 10 - 18
fsub 10,10,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 590: 

# input line 591:   x3 = h3 - y3
# x3#3 = h3#2 - y3#4
# double#13 = double#10 - double#22
# 12 = 9 - 21
fsub 12,9,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 592: 

# input line 593:   x2 = h2 - y2
# x2#4 = h2 - y2#4
# double#14 = double#9 - double#23
# 13 = 8 - 22
fsub 13,8,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 29
# live flags values: 0

# input line 594: 

# input line 595:   x1 += y7 * scale
# x1#4 = x1#3 + y7#4 * scale
# double#9 = double#20 + double#7 * double#1
# 8 = 19 + 6 * 0
fmadd 8,6,0,19
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 28
# live flags values: 0

# input line 596: 

# input line 597:   x0 += y6 * scale
# x0#5 = x0#4 + y6#4 * scale
# double#10 = double#21 + double#15 * double#1
# 9 = 20 + 14 * 0
fmadd 9,14,0,20
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 598: 

# input line 599:   x7 += y5
# x7#4 = x7#3 + y5#4
# double#2 = double#2 + double#18
# 1 = 1 + 17
fadd 1,1,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 600: 

# input line 601:   x6 += y4
# x6#5 = x6#4 + y4#4
# double#7 = double#8 + double#19
# 6 = 7 + 18
fadd 6,7,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 602: 

# input line 603:   x5 += y3
# x5#4 = x5#3 + y3#4
# double#8 = double#12 + double#22
# 7 = 11 + 21
fadd 7,11,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 604: 

# input line 605:   x4 += y2
# x4#5 = x4#4 + y2#4
# double#11 = double#11 + double#23
# 10 = 10 + 22
fadd 10,10,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 606: 

# input line 607:   x3 += y1
# x3#4 = x3#3 + y1#4
# double#12 = double#13 + double#16
# 11 = 12 + 15
fadd 11,12,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 608: 

# input line 609:   x2 += y0
# x2#5 = x2#4 + y0#4
# double#13 = double#14 + double#17
# 12 = 13 + 16
fadd 12,13,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 610: 

# input line 611:   x0 += x1
# x0#6 = x0#5 + x1#4
# double#14 = double#10 + double#9
# 13 = 9 + 8
fadd 13,9,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 20
# live flags values: 0

# input line 612: 

# input line 613:   x6 += x7
# x6#6 = x6#5 + x7#4
# double#15 = double#7 + double#2
# 14 = 6 + 1
fadd 14,6,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 19
# live flags values: 0

# input line 614: 

# input line 615:   x4 += x5
# x4#6 = x4#5 + x5#4
# double#18 = double#11 + double#8
# 17 = 10 + 7
fadd 17,10,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 616: 

# input line 617:   x2 += x3
# x2#6 = x2#5 + x3#4
# double#16 = double#13 + double#12
# 15 = 12 + 11
fadd 15,12,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 17
# live flags values: 0

# input line 618: 

# input line 619:   h7 = r3high * x0
# h7#5 = r3high#2 * x0#6
# double#13 = double#26 * double#14
# 12 = 25 * 13
fmul 12,25,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 18
# live flags values: 0

# input line 620: 

# input line 621:   h6 = r3low * x0
# h6#5 = r3low#3 * x0#6
# double#12 = double#24 * double#14
# 11 = 23 * 13
fmul 11,23,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 19
# live flags values: 0

# input line 622: 

# input line 623:   h5 = r2high * x0
# h5#7 = r2high#2 * x0#6
# double#11 = double#31 * double#14
# 10 = 30 * 13
fmul 10,30,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 20
# live flags values: 0

# input line 624: 

# input line 625:   h4 = r2low * x0
# h4#5 = r2low#3 * x0#6
# double#10 = double#30 * double#14
# 9 = 29 * 13
fmul 9,29,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 626: 

# input line 627:   h3 = r1high * x0
# h3#7 = r1high#2 * x0#6
# double#9 = double#29 * double#14
# 8 = 28 * 13
fmul 8,28,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 22
# live flags values: 0

# input line 628: 

# input line 629:   h2 = r1low * x0
# h2#5 = r1low#3 * x0#6
# double#8 = double#28 * double#14
# 7 = 27 * 13
fmul 7,27,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 630: 

# input line 631:   h1 = r0high * x0
# h1#7 = r0high#2 * x0#6
# double#7 = double#27 * double#14
# 6 = 26 * 13
fmul 6,26,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 632: 

# input line 633:   h0 = r0low * x0
# h0#7 = r0low#3 * x0#6
# double#2 = double#25 * double#14
# 1 = 24 * 13
fmul 1,24,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 634: 

# input line 635:   h7 += r2high * x2
# h7#6 = h7#5 + r2high#2 * x2#6
# double#13 = double#13 + double#31 * double#16
# 12 = 12 + 30 * 15
fmadd 12,30,15,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 636: 

# input line 637:   h6 += r2low * x2
# h6#6 = h6#5 + r2low#3 * x2#6
# double#12 = double#12 + double#30 * double#16
# 11 = 11 + 29 * 15
fmadd 11,29,15,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 638: 

# input line 639:   h5 += r1high * x2
# h5#8 = h5#7 + r1high#2 * x2#6
# double#11 = double#11 + double#29 * double#16
# 10 = 10 + 28 * 15
fmadd 10,28,15,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 640: 

# input line 641:   h4 += r1low * x2
# h4#6 = h4#5 + r1low#3 * x2#6
# double#10 = double#10 + double#28 * double#16
# 9 = 9 + 27 * 15
fmadd 9,27,15,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 642: 

# input line 643:   h3 += r0high * x2
# h3#8 = h3#7 + r0high#2 * x2#6
# double#9 = double#9 + double#27 * double#16
# 8 = 8 + 26 * 15
fmadd 8,26,15,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 644: 

# input line 645:   h2 += r0low * x2
# h2#6 = h2#5 + r0low#3 * x2#6
# double#8 = double#8 + double#25 * double#16
# 7 = 7 + 24 * 15
fmadd 7,24,15,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 646: 

# input line 647:   h7 += r1high * x4
# h7#7 = h7#6 + r1high#2 * x4#6
# double#13 = double#13 + double#29 * double#18
# 12 = 12 + 28 * 17
fmadd 12,28,17,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 648: 

# input line 649:   h6 += r1low * x4
# h6#7 = h6#6 + r1low#3 * x4#6
# double#12 = double#12 + double#28 * double#18
# 11 = 11 + 27 * 17
fmadd 11,27,17,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 650: 

# input line 651:   load sr3high
# sr3high#4 = sr3high@stack
# double#17 = mem64#28
# 16 = 344(1)
lfd 16,344(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 652:   h5 += r0high * x4
# h5#9 = h5#8 + r0high#2 * x4#6
# double#11 = double#11 + double#27 * double#18
# 10 = 10 + 26 * 17
fmadd 10,26,17,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 653: 

# input line 654:   load sr3low
# sr3low#4 = sr3low@stack
# double#21 = mem64#27
# 20 = 336(1)
lfd 20,336(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 655:   h4 += r0low * x4
# h4#7 = h4#6 + r0low#3 * x4#6
# double#10 = double#10 + double#25 * double#18
# 9 = 9 + 24 * 17
fmadd 9,24,17,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 656: 

# input line 657:   h7 += r0high * x6
# h7 = h7#7 + r0high#2 * x6#6
# double#14 = double#13 + double#27 * double#15
# 13 = 12 + 26 * 14
fmadd 13,26,14,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 658: 

# input line 659:   h6 += r0low * x6
# h6 = h6#7 + r0low#3 * x6#6
# double#13 = double#12 + double#25 * double#15
# 12 = 11 + 24 * 14
fmadd 12,24,14,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 660: 

# input line 661:   h1 += sr3high * x2
# h1#8 = h1#7 + sr3high#4 * x2#6
# double#7 = double#7 + double#17 * double#16
# 6 = 6 + 16 * 15
fmadd 6,16,15,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 662: 

# input line 663:   h0 += sr3low * x2
# h0#8 = h0#7 + sr3low#4 * x2#6
# double#2 = double#2 + double#21 * double#16
# 1 = 1 + 20 * 15
fmadd 1,20,15,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 664: 

# input line 665:   load sr2high
# sr2high#4 = sr2high@stack
# double#20 = mem64#26
# 19 = 328(1)
lfd 19,328(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 666:   h3 += sr3high * x4
# h3#9 = h3#8 + sr3high#4 * x4#6
# double#9 = double#9 + double#17 * double#18
# 8 = 8 + 16 * 17
fmadd 8,16,17,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 667: 

# input line 668:   load sr2low
# sr2low#4 = sr2low@stack
# double#19 = mem64#25
# 18 = 320(1)
lfd 18,320(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 669:   h2 += sr3low * x4
# h2#7 = h2#6 + sr3low#4 * x4#6
# double#8 = double#8 + double#21 * double#18
# 7 = 7 + 20 * 17
fmadd 7,20,17,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 670: 

# input line 671:   h5 += sr3high * x6
# h5 = h5#9 + sr3high#4 * x6#6
# double#12 = double#11 + double#17 * double#15
# 11 = 10 + 16 * 14
fmadd 11,16,14,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 672: 

# input line 673:   load sr1high
# sr1high#4 = sr1high@stack
# double#17 = mem64#24
# 16 = 312(1)
lfd 16,312(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 674:   h4 += sr3low * x6
# h4 = h4#7 + sr3low#4 * x6#6
# double#11 = double#10 + double#21 * double#15
# 10 = 9 + 20 * 14
fmadd 10,20,14,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 675: 

# input line 676:   load sr1low
# sr1low#4 = sr1low@stack
# double#16 = mem64#23
# 15 = 304(1)
lfd 15,304(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 677:   h1 += sr2high * x4
# h1#9 = h1#8 + sr2high#4 * x4#6
# double#7 = double#7 + double#20 * double#18
# 6 = 6 + 19 * 17
fmadd 6,19,17,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 27
# live flags values: 0

# input line 678: 

# input line 679:   h0 += sr2low * x4
# h0#9 = h0#8 + sr2low#4 * x4#6
# double#2 = double#2 + double#19 * double#18
# 1 = 1 + 18 * 17
fmadd 1,18,17,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 26
# live flags values: 0

# input line 680: 

# input line 681:   h3 += sr2high * x6
# h3 = h3#9 + sr2high#4 * x6#6
# double#10 = double#9 + double#20 * double#15
# 9 = 8 + 19 * 14
fmadd 9,19,14,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 25
# live flags values: 0

# input line 682: 

# input line 683:   h2 += sr2low * x6
# h2 = h2#7 + sr2low#4 * x6#6
# double#9 = double#8 + double#19 * double#15
# 8 = 7 + 18 * 14
fmadd 8,18,14,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 24
# live flags values: 0

# input line 684: 

# input line 685:   h1 += sr1high * x6
# h1 = h1#9 + sr1high#4 * x6#6
# double#8 = double#7 + double#17 * double#15
# 7 = 6 + 16 * 14
fmadd 7,16,14,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 23
# live flags values: 0

# input line 686: 

# input line 687:   h0 += sr1low * x6
# h0 = h0#9 + sr1low#4 * x6#6
# double#7 = double#2 + double#16 * double#15
# 6 = 1 + 15 * 14
fmadd 6,15,14,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 688: 

# input line 689: 

# input line 690: addatmost15bytes
.label.addatmost15bytes:

# input line 691: 

# input line 692: lgeflags unsigned l - 0
# flags unsigned l - 0
# flags unsigned int32#5 - 0
# flags unsigned 7 - 0
cmplwi 7,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 15
# live double values: 21
# live flags values: 1

# input line 693: 

# input line 694:   lbelow2 = l - 2
# lbelow2 = l - 2
# int32#2 = int32#5 - 2
# 4 = 7 - 2
addi 4,7,-2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 16
# live double values: 21
# live flags values: 1

# input line 695: 

# input line 696:   lbelow3 = l - 3
# lbelow3 = l - 3
# int32#7 = int32#5 - 3
# 9 = 7 - 3
addi 9,7,-3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 17
# live double values: 21
# live flags values: 1

# input line 697: 

# input line 698:   (int32) lbelow2 >>= 31
# lbelow2#2 = (int32) lbelow2 >> 31
# int32#22 = (int32) int32#2 >> 31
# 24 = (int32) 4 >> 31
srawi 24,4,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 17
# live double values: 21
# live flags values: 1

# input line 699:   lbelow4 = l - 4
# lbelow4 = l - 4
# int32#10 = int32#5 - 4
# 12 = 7 - 4
addi 12,7,-4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 21
# live flags values: 1

# input line 700: goto nomorebytes if ==
beq .label.nomorebytes

# input line 701: 

# input line 702:   m00 = *(uchar *) (m + 0)
# m00 = *(uchar *) (m + 0)
# int32#8 = *(uchar *) (int32#4 + 0)
# 10 = *(uchar *) (6 + 0)
lbz 10,0(6)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 703:   (int32) lbelow3 >>= 31
# lbelow3#2 = (int32) lbelow3 >> 31
# int32#23 = (int32) int32#7 >> 31
# 25 = (int32) 9 >> 31
srawi 25,9,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 704:   m += lbelow2
# m#8 = m + lbelow2#2
# int32#2 = int32#4 + int32#22
# 4 = 6 + 24
add 4,6,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 705: 

# input line 706:   m01 = *(uchar *) (m + 1)
# m01 = *(uchar *) (m#8 + 1)
# int32#9 = *(uchar *) (int32#2 + 1)
# 11 = *(uchar *) (4 + 1)
lbz 11,1(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 707:   (int32) lbelow4 >>= 31
# lbelow4#2 = (int32) lbelow4 >> 31
# int32#24 = (int32) int32#10 >> 31
# 26 = (int32) 12 >> 31
srawi 26,12,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 708:   m += lbelow3
# m#9 = m#8 + lbelow3#2
# int32#2 = int32#2 + int32#23
# 4 = 4 + 25
add 4,4,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 709: 

# input line 710:   m02 = *(uchar *) (m + 2)
# m02 = *(uchar *) (m#9 + 2)
# int32#10 = *(uchar *) (int32#2 + 2)
# 12 = *(uchar *) (4 + 2)
lbz 12,2(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 711:   m += lbelow4
# m#10 = m#9 + lbelow4#2
# int32#2 = int32#2 + int32#24
# 4 = 4 + 26
add 4,4,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 712:   m0 = 0
# m0#4 = 0
# int32#4 = 0
# 6 = 0
li 6,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 713: 

# input line 714:   m03 = *(uchar *) (m + 3)
# m03 = *(uchar *) (m#10 + 3)
# int32#21 = *(uchar *) (int32#2 + 3)
# 23 = *(uchar *) (4 + 3)
lbz 23,3(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 715:   m1 = 0
# m1#4 = 0
# int32#7 = 0
# 9 = 0
li 9,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 716: 

# input line 717:   m0 += m00
# m0#5 = m0#4 + m00
# int32#4 = int32#4 + int32#8
# 6 = 6 + 10
add 6,6,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 718:   m01 &= ~lbelow2
# m01#2 = m01 &~ lbelow2#2
# int32#8 = int32#9 &~ int32#22
# 10 = 11 &~ 24
andc 10,11,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 719: 

# input line 720:   m02 &= ~lbelow3
# m02#2 = m02 &~ lbelow3#2
# int32#9 = int32#10 &~ int32#23
# 11 = 12 &~ 25
andc 11,12,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 721:   m01 -= lbelow2
# m01#3 = m01#2 - lbelow2#2
# int32#8 = int32#8 - int32#22
# 10 = 10 - 24
subf 10,24,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 722: 

# input line 723:   m01 <<= 8
# m01#4 = m01#3 << 8
# int32#8 = int32#8 << 8
# 10 = 10 << 8
slwi 10,10,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 724:   m03 &= ~lbelow4
# m03#2 = m03 &~ lbelow4#2
# int32#10 = int32#21 &~ int32#24
# 12 = 23 &~ 26
andc 12,23,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 725: 

# input line 726:   m0 += m01
# m0#6 = m0#5 + m01#4
# int32#4 = int32#4 + int32#8
# 6 = 6 + 10
add 6,6,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 727:   lbelow2 -= lbelow3
# lbelow2#3 = lbelow2#2 - lbelow3#2
# int32#8 = int32#22 - int32#23
# 10 = 24 - 25
subf 10,25,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 728: 

# input line 729:   m02 += lbelow2
# m02#3 = m02#2 + lbelow2#3
# int32#8 = int32#9 + int32#8
# 10 = 11 + 10
add 10,11,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 730:   lbelow3 -= lbelow4
# lbelow3#3 = lbelow3#2 - lbelow4#2
# int32#9 = int32#23 - int32#24
# 11 = 25 - 26
subf 11,26,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 731: 

# input line 732:   m02 <<= 16
# m02#4 = m02#3 << 16
# int32#8 = int32#8 << 16
# 10 = 10 << 16
slwi 10,10,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 733:   m03 += lbelow3
# m03#3 = m03#2 + lbelow3#3
# int32#9 = int32#10 + int32#9
# 11 = 12 + 11
add 11,12,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 734: 

# input line 735:   m03 <<= 24
# m03#4 = m03#3 << 24
# int32#9 = int32#9 << 24
# 11 = 11 << 24
slwi 11,11,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 736:   m0 += m02
# m0#7 = m0#6 + m02#4
# int32#4 = int32#4 + int32#8
# 6 = 6 + 10
add 6,6,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 737: 

# input line 738:   m0 += m03
# m0#8 = m0#7 + m03#4
# int32#4 = int32#4 + int32#9
# 6 = 6 + 11
add 6,6,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 21
# live flags values: 0

# input line 739:   lbelow5 = l - 5
# lbelow5 = l - 5
# int32#8 = int32#5 - 5
# 10 = 7 - 5
addi 10,7,-5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 740: 

# input line 741:   lbelow6 = l - 6
# lbelow6 = l - 6
# int32#9 = int32#5 - 6
# 11 = 7 - 6
addi 11,7,-6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 742:   lbelow7 = l - 7
# lbelow7 = l - 7
# int32#10 = int32#5 - 7
# 12 = 7 - 7
addi 12,7,-7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 743: 

# input line 744:   (int32) lbelow5 >>= 31
# lbelow5#2 = (int32) lbelow5 >> 31
# int32#23 = (int32) int32#8 >> 31
# 25 = (int32) 10 >> 31
srawi 25,10,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 745:   lbelow8 = l - 8
# lbelow8 = l - 8
# int32#21 = int32#5 - 8
# 23 = 7 - 8
addi 23,7,-8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 746: 

# input line 747:   (int32) lbelow6 >>= 31
# lbelow6#2 = (int32) lbelow6 >> 31
# int32#25 = (int32) int32#9 >> 31
# 27 = (int32) 11 >> 31
srawi 27,11,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 748:   m += lbelow5
# m#11 = m#10 + lbelow5#2
# int32#2 = int32#2 + int32#23
# 4 = 4 + 25
add 4,4,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 749: 

# input line 750:   m10 = *(uchar *) (m + 4)
# m10 = *(uchar *) (m#11 + 4)
# int32#8 = *(uchar *) (int32#2 + 4)
# 10 = *(uchar *) (4 + 4)
lbz 10,4(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 751:   (int32) lbelow7 >>= 31
# lbelow7#2 = (int32) lbelow7 >> 31
# int32#26 = (int32) int32#10 >> 31
# 28 = (int32) 12 >> 31
srawi 28,12,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 752:   m += lbelow6
# m#12 = m#11 + lbelow6#2
# int32#2 = int32#2 + int32#25
# 4 = 4 + 27
add 4,4,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 753: 

# input line 754:   m11 = *(uchar *) (m + 5)
# m11 = *(uchar *) (m#12 + 5)
# int32#9 = *(uchar *) (int32#2 + 5)
# 11 = *(uchar *) (4 + 5)
lbz 11,5(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 755:   (int32) lbelow8 >>= 31
# lbelow8#2 = (int32) lbelow8 >> 31
# int32#27 = (int32) int32#21 >> 31
# 29 = (int32) 23 >> 31
srawi 29,23,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 756:   m += lbelow7
# m#13 = m#12 + lbelow7#2
# int32#2 = int32#2 + int32#26
# 4 = 4 + 28
add 4,4,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 757: 

# input line 758:   m12 = *(uchar *) (m + 6)
# m12 = *(uchar *) (m#13 + 6)
# int32#10 = *(uchar *) (int32#2 + 6)
# 12 = *(uchar *) (4 + 6)
lbz 12,6(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 759:   m += lbelow8
# m#14 = m#13 + lbelow8#2
# int32#2 = int32#2 + int32#27
# 4 = 4 + 29
add 4,4,29
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 760: 

# input line 761:   m13 = *(uchar *) (m + 7)
# m13 = *(uchar *) (m#14 + 7)
# int32#21 = *(uchar *) (int32#2 + 7)
# 23 = *(uchar *) (4 + 7)
lbz 23,7(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 762:   m10 &= ~lbelow5
# m10#2 = m10 &~ lbelow5#2
# int32#8 = int32#8 &~ int32#23
# 10 = 10 &~ 25
andc 10,10,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 763:   lbelow4 -= lbelow5
# lbelow4#3 = lbelow4#2 - lbelow5#2
# int32#22 = int32#24 - int32#23
# 24 = 26 - 25
subf 24,25,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 764: 

# input line 765:   m10 += lbelow4
# m10#3 = m10#2 + lbelow4#3
# int32#8 = int32#8 + int32#22
# 10 = 10 + 24
add 10,10,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 766:   lbelow5 -= lbelow6
# lbelow5#3 = lbelow5#2 - lbelow6#2
# int32#22 = int32#23 - int32#25
# 24 = 25 - 27
subf 24,27,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 767: 

# input line 768:   m11 &= ~lbelow6
# m11#2 = m11 &~ lbelow6#2
# int32#9 = int32#9 &~ int32#25
# 11 = 11 &~ 27
andc 11,11,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 769:   m11 += lbelow5
# m11#3 = m11#2 + lbelow5#3
# int32#9 = int32#9 + int32#22
# 11 = 11 + 24
add 11,11,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 770: 

# input line 771:   m11 <<= 8
# m11#4 = m11#3 << 8
# int32#9 = int32#9 << 8
# 11 = 11 << 8
slwi 11,11,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 772:   m1 += m10
# m1#5 = m1#4 + m10#3
# int32#7 = int32#7 + int32#8
# 9 = 9 + 10
add 9,9,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 773: 

# input line 774:   m1 += m11
# m1#6 = m1#5 + m11#4
# int32#7 = int32#7 + int32#9
# 9 = 9 + 11
add 9,9,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 775:   m12 &= ~lbelow7
# m12#2 = m12 &~ lbelow7#2
# int32#8 = int32#10 &~ int32#26
# 10 = 12 &~ 28
andc 10,12,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 776: 

# input line 777:   lbelow6 -= lbelow7
# lbelow6#3 = lbelow6#2 - lbelow7#2
# int32#10 = int32#25 - int32#26
# 12 = 27 - 28
subf 12,28,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 778:   m13 &= ~lbelow8
# m13#2 = m13 &~ lbelow8#2
# int32#9 = int32#21 &~ int32#27
# 11 = 23 &~ 29
andc 11,23,29
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 779: 

# input line 780:   m12 += lbelow6
# m12#3 = m12#2 + lbelow6#3
# int32#8 = int32#8 + int32#10
# 10 = 10 + 12
add 10,10,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 781:   lbelow7 -= lbelow8
# lbelow7#3 = lbelow7#2 - lbelow8#2
# int32#10 = int32#26 - int32#27
# 12 = 28 - 29
subf 12,29,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 782: 

# input line 783:   m12 <<= 16
# m12#4 = m12#3 << 16
# int32#8 = int32#8 << 16
# 10 = 10 << 16
slwi 10,10,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 784:   m13 += lbelow7
# m13#3 = m13#2 + lbelow7#3
# int32#9 = int32#9 + int32#10
# 11 = 11 + 12
add 11,11,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 785: 

# input line 786:   m13 <<= 24
# m13#4 = m13#3 << 24
# int32#9 = int32#9 << 24
# 11 = 11 << 24
slwi 11,11,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 787:   m1 += m12
# m1#7 = m1#6 + m12#4
# int32#7 = int32#7 + int32#8
# 9 = 9 + 10
add 9,9,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 788: 

# input line 789:   m1 += m13
# m1#8 = m1#7 + m13#4
# int32#7 = int32#7 + int32#9
# 9 = 9 + 11
add 9,9,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 21
# live flags values: 0

# input line 790:   m2 = 0
# m2#4 = 0
# int32#8 = 0
# 10 = 0
li 10,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 791: 

# input line 792:   lbelow9 = l - 9
# lbelow9 = l - 9
# int32#10 = int32#5 - 9
# 12 = 7 - 9
addi 12,7,-9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 793:   m3 = 0
# m3#4 = 0
# int32#9 = 0
# 11 = 0
li 11,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 794: 

# input line 795:   lbelow10 = l - 10
# lbelow10 = l - 10
# int32#21 = int32#5 - 10
# 23 = 7 - 10
addi 23,7,-10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 796:   lbelow11 = l - 11
# lbelow11 = l - 11
# int32#22 = int32#5 - 11
# 24 = 7 - 11
addi 24,7,-11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 797: 

# input line 798:   (int32) lbelow9 >>= 31
# lbelow9#2 = (int32) lbelow9 >> 31
# int32#25 = (int32) int32#10 >> 31
# 27 = (int32) 12 >> 31
srawi 27,12,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 799:   lbelow12 = l - 12
# lbelow12 = l - 12
# int32#23 = int32#5 - 12
# 25 = 7 - 12
addi 25,7,-12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 800: 

# input line 801:   (int32) lbelow10 >>= 31
# lbelow10#2 = (int32) lbelow10 >> 31
# int32#26 = (int32) int32#21 >> 31
# 28 = (int32) 23 >> 31
srawi 28,23,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 802:   m += lbelow9
# m#15 = m#14 + lbelow9#2
# int32#2 = int32#2 + int32#25
# 4 = 4 + 27
add 4,4,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 803: 

# input line 804:   m20 = *(uchar *) (m + 8)
# m20 = *(uchar *) (m#15 + 8)
# int32#10 = *(uchar *) (int32#2 + 8)
# 12 = *(uchar *) (4 + 8)
lbz 12,8(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 805:   (int32) lbelow11 >>= 31
# lbelow11#2 = (int32) lbelow11 >> 31
# int32#28 = (int32) int32#22 >> 31
# 30 = (int32) 24 >> 31
srawi 30,24,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 806:   m += lbelow10
# m#16 = m#15 + lbelow10#2
# int32#2 = int32#2 + int32#26
# 4 = 4 + 28
add 4,4,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 807: 

# input line 808:   m21 = *(uchar *) (m + 9)
# m21 = *(uchar *) (m#16 + 9)
# int32#21 = *(uchar *) (int32#2 + 9)
# 23 = *(uchar *) (4 + 9)
lbz 23,9(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 809:   (int32) lbelow12 >>= 31
# lbelow12#2 = (int32) lbelow12 >> 31
# int32#29 = (int32) int32#23 >> 31
# 31 = (int32) 25 >> 31
srawi 31,25,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 810:   m += lbelow11
# m#17 = m#16 + lbelow11#2
# int32#2 = int32#2 + int32#28
# 4 = 4 + 30
add 4,4,30
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 811: 

# input line 812:   m22 = *(uchar *) (m + 10)
# m22 = *(uchar *) (m#17 + 10)
# int32#22 = *(uchar *) (int32#2 + 10)
# 24 = *(uchar *) (4 + 10)
lbz 24,10(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 27
# live double values: 21
# live flags values: 0

# input line 813:   m += lbelow12
# m#18 = m#17 + lbelow12#2
# int32#2 = int32#2 + int32#29
# 4 = 4 + 31
add 4,4,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 27
# live double values: 21
# live flags values: 0

# input line 814: 

# input line 815:   m23 = *(uchar *) (m + 11)
# m23 = *(uchar *) (m#18 + 11)
# int32#23 = *(uchar *) (int32#2 + 11)
# 25 = *(uchar *) (4 + 11)
lbz 25,11(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 28
# live double values: 21
# live flags values: 0

# input line 816:   m20 &= ~lbelow9
# m20#2 = m20 &~ lbelow9#2
# int32#10 = int32#10 &~ int32#25
# 12 = 12 &~ 27
andc 12,12,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 28
# live double values: 21
# live flags values: 0

# input line 817:   lbelow8 -= lbelow9
# lbelow8#3 = lbelow8#2 - lbelow9#2
# int32#24 = int32#27 - int32#25
# 26 = 29 - 27
subf 26,27,29
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 28
# live double values: 21
# live flags values: 0

# input line 818: 

# input line 819:   m20 += lbelow8
# m20#3 = m20#2 + lbelow8#3
# int32#10 = int32#10 + int32#24
# 12 = 12 + 26
add 12,12,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 27
# live double values: 21
# live flags values: 0

# input line 820:   lbelow9 -= lbelow10
# lbelow9#3 = lbelow9#2 - lbelow10#2
# int32#24 = int32#25 - int32#26
# 26 = 27 - 28
subf 26,28,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 27
# live double values: 21
# live flags values: 0

# input line 821: 

# input line 822:   m21 &= ~lbelow10
# m21#2 = m21 &~ lbelow10#2
# int32#21 = int32#21 &~ int32#26
# 23 = 23 &~ 28
andc 23,23,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 27
# live double values: 21
# live flags values: 0

# input line 823:   m21 += lbelow9
# m21#3 = m21#2 + lbelow9#3
# int32#21 = int32#21 + int32#24
# 23 = 23 + 26
add 23,23,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 824: 

# input line 825:   m21 <<= 8
# m21#4 = m21#3 << 8
# int32#21 = int32#21 << 8
# 23 = 23 << 8
slwi 23,23,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 26
# live double values: 21
# live flags values: 0

# input line 826:   m2 += m20
# m2#5 = m2#4 + m20#3
# int32#8 = int32#8 + int32#10
# 10 = 10 + 12
add 10,10,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 827: 

# input line 828:   m2 += m21
# m2#6 = m2#5 + m21#4
# int32#8 = int32#8 + int32#21
# 10 = 10 + 23
add 10,10,23
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 829:   m22 &= ~lbelow11
# m22#2 = m22 &~ lbelow11#2
# int32#10 = int32#22 &~ int32#28
# 12 = 24 &~ 30
andc 12,24,30
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 830: 

# input line 831:   lbelow10 -= lbelow11
# lbelow10#3 = lbelow10#2 - lbelow11#2
# int32#22 = int32#26 - int32#28
# 24 = 28 - 30
subf 24,30,28
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 832:   m23 &= ~lbelow12
# m23#2 = m23 &~ lbelow12#2
# int32#21 = int32#23 &~ int32#29
# 23 = 25 &~ 31
andc 23,25,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 833: 

# input line 834:   m22 += lbelow10
# m22#3 = m22#2 + lbelow10#3
# int32#10 = int32#10 + int32#22
# 12 = 12 + 24
add 12,12,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 835:   lbelow11 -= lbelow12
# lbelow11#3 = lbelow11#2 - lbelow12#2
# int32#22 = int32#28 - int32#29
# 24 = 30 - 31
subf 24,31,30
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 836: 

# input line 837:   m22 <<= 16
# m22#4 = m22#3 << 16
# int32#10 = int32#10 << 16
# 12 = 12 << 16
slwi 12,12,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 838:   m23 += lbelow11
# m23#3 = m23#2 + lbelow11#3
# int32#21 = int32#21 + int32#22
# 23 = 23 + 24
add 23,23,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 839: 

# input line 840:   m23 <<= 24
# m23#4 = m23#3 << 24
# int32#21 = int32#21 << 24
# 23 = 23 << 24
slwi 23,23,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 841:   m2 += m22
# m2#7 = m2#6 + m22#4
# int32#8 = int32#8 + int32#10
# 10 = 10 + 12
add 10,10,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 842: 

# input line 843:   lbelow13 = l - 13
# lbelow13 = l - 13
# int32#10 = int32#5 - 13
# 12 = 7 - 13
addi 12,7,-13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 844: 

# input line 845:   (int32) lbelow13 >>= 31
# lbelow13#2 = (int32) lbelow13 >> 31
# int32#23 = (int32) int32#10 >> 31
# 25 = (int32) 12 >> 31
srawi 25,12,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 846:   lbelow14 = l - 14
# lbelow14 = l - 14
# int32#10 = int32#5 - 14
# 12 = 7 - 14
addi 12,7,-14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 847: 

# input line 848:   (int32) lbelow14 >>= 31
# lbelow14#2 = (int32) lbelow14 >> 31
# int32#24 = (int32) int32#10 >> 31
# 26 = (int32) 12 >> 31
srawi 26,12,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 849:   m += lbelow13
# m#19 = m#18 + lbelow13#2
# int32#2 = int32#2 + int32#23
# 4 = 4 + 25
add 4,4,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 850:   lbelow15 = l - 15
# lbelow15 = l - 15
# int32#5 = int32#5 - 15
# 7 = 7 - 15
addi 7,7,-15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 851: 

# input line 852:   m30 = *(uchar *) (m + 12)
# m30 = *(uchar *) (m#19 + 12)
# int32#10 = *(uchar *) (int32#2 + 12)
# 12 = *(uchar *) (4 + 12)
lbz 12,12(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 853:   (int32) lbelow15 >>= 31
# lbelow15#2 = (int32) lbelow15 >> 31
# int32#25 = (int32) int32#5 >> 31
# 27 = (int32) 7 >> 31
srawi 27,7,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 854:   m += lbelow14
# m#20 = m#19 + lbelow14#2
# int32#2 = int32#2 + int32#24
# 4 = 4 + 26
add 4,4,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 855: 

# input line 856:   m31 = *(uchar *) (m + 13)
# m31 = *(uchar *) (m#20 + 13)
# int32#22 = *(uchar *) (int32#2 + 13)
# 24 = *(uchar *) (4 + 13)
lbz 24,13(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 857:   m += lbelow15
# m#21 = m#20 + lbelow15#2
# int32#2 = int32#2 + int32#25
# 4 = 4 + 27
add 4,4,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 25
# live double values: 21
# live flags values: 0

# input line 858:   m2 += m23
# m2#8 = m2#7 + m23#4
# int32#5 = int32#8 + int32#21
# 7 = 10 + 23
add 7,10,23
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 859: 

# input line 860:   m32 = *(uchar *) (m + 14)
# m32 = *(uchar *) (m#21 + 14)
# int32#21 = *(uchar *) (int32#2 + 14)
# 23 = *(uchar *) (4 + 14)
lbz 23,14(4)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 861:   m30 &= ~lbelow13
# m30#2 = m30 &~ lbelow13#2
# int32#2 = int32#10 &~ int32#23
# 4 = 12 &~ 25
andc 4,12,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 862:   lbelow12 -= lbelow13
# lbelow12#3 = lbelow12#2 - lbelow13#2
# int32#8 = int32#29 - int32#23
# 10 = 31 - 25
subf 10,25,31
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 24
# live double values: 21
# live flags values: 0

# input line 863: 

# input line 864:   m30 += lbelow12
# m30#3 = m30#2 + lbelow12#3
# int32#2 = int32#2 + int32#8
# 4 = 4 + 10
add 4,4,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 865:   lbelow13 -= lbelow14
# lbelow13#3 = lbelow13#2 - lbelow14#2
# int32#10 = int32#23 - int32#24
# 12 = 25 - 26
subf 12,26,25
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 23
# live double values: 21
# live flags values: 0

# input line 866: 

# input line 867:   m3 += m30
# m3#5 = m3#4 + m30#3
# int32#2 = int32#9 + int32#2
# 4 = 11 + 4
add 4,11,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 868:   m31 &= ~lbelow14
# m31#2 = m31 &~ lbelow14#2
# int32#8 = int32#22 &~ int32#24
# 10 = 24 &~ 26
andc 10,24,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 22
# live double values: 21
# live flags values: 0

# input line 869: 

# input line 870:   m31 += lbelow13
# m31#3 = m31#2 + lbelow13#3
# int32#8 = int32#8 + int32#10
# 10 = 10 + 12
add 10,10,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 871:   m32 &= ~lbelow15
# m32#2 = m32 &~ lbelow15#2
# int32#9 = int32#21 &~ int32#25
# 11 = 23 &~ 27
andc 11,23,27
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 872: 

# input line 873:   m31 <<= 8
# m31#4 = m31#3 << 8
# int32#8 = int32#8 << 8
# 10 = 10 << 8
slwi 10,10,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 874:   lbelow14 -= lbelow15
# lbelow14#3 = lbelow14#2 - lbelow15#2
# int32#10 = int32#24 - int32#25
# 12 = 26 - 27
subf 12,27,26
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 21
# live double values: 21
# live flags values: 0

# input line 875: 

# input line 876:   m3 += m31
# m3#6 = m3#5 + m31#4
# int32#2 = int32#2 + int32#8
# 4 = 4 + 10
add 4,4,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 20
# live double values: 21
# live flags values: 0

# input line 877:   m32 += lbelow14
# m32#3 = m32#2 + lbelow14#3
# int32#8 = int32#9 + int32#10
# 10 = 11 + 12
add 10,11,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 878: 

# input line 879:   m32 <<= 16
# m32#4 = m32#3 << 16
# int32#8 = int32#8 << 16
# 10 = 10 << 16
slwi 10,10,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 880:   m33 = lbelow15 + 1
# m33 = lbelow15#2 + 1
# int32#9 = int32#25 + 1
# 11 = 27 + 1
addi 11,27,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 881: 

# input line 882:   m33 <<= 24
# m33#2 = m33 << 24
# int32#9 = int32#9 << 24
# 11 = 11 << 24
slwi 11,11,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 19
# live double values: 21
# live flags values: 0

# input line 883:   m3 += m32
# m3#7 = m3#6 + m32#4
# int32#2 = int32#2 + int32#8
# 4 = 4 + 10
add 4,4,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 18
# live double values: 21
# live flags values: 0

# input line 884: 

# input line 885:   m3 += m33
# m3#8 = m3#7 + m33#2
# int32#2 = int32#2 + int32#9
# 4 = 4 + 11
add 4,4,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 17
# live double values: 21
# live flags values: 0

# input line 886: 

# input line 887:   d0 bottom = m0
# d0#4 bottom = m0#8
# mem64#19 bottom = int32#4
# 272(1) bottom = 6
stw 6,276(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 16
# live double values: 21
# live flags values: 0

# input line 888:   d1 bottom = m1
# d1#4 bottom = m1#8
# mem64#20 bottom = int32#7
# 280(1) bottom = 9
stw 9,284(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 889:   d2 bottom = m2
# d2#4 bottom = m2#8
# mem64#21 bottom = int32#5
# 288(1) bottom = 7
stw 7,292(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 14
# live double values: 21
# live flags values: 0

# input line 890:   d3 bottom = m3
# d3#4 bottom = m3#8
# mem64#22 bottom = int32#2
# 296(1) bottom = 4
stw 4,300(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 13
# live double values: 21
# live flags values: 0

# input line 891: 

# input line 892:   m0 = 65536 * 0x4338
# m0#9 = 0x4338
# int32#2 = 0x4338
# 4 = 0x4338
lis 4,0x4338
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 14
# live double values: 21
# live flags values: 0

# input line 893:   m1 = 65536 * 0x4538
# m1#9 = 0x4538
# int32#4 = 0x4538
# 6 = 0x4538
lis 6,0x4538
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 894:   m2 = 65536 * 0x4738
# m2#9 = 0x4738
# int32#5 = 0x4738
# 7 = 0x4738
lis 7,0x4738
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 16
# live double values: 21
# live flags values: 0

# input line 895:   m3 = 65536 * 0x4938
# m3#9 = 0x4938
# int32#7 = 0x4938
# 9 = 0x4938
lis 9,0x4938
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 17
# live double values: 21
# live flags values: 0

# input line 896: 

# input line 897:   inplace d0 top = m0
# d0#4 top = m0#9
# mem64#19 top = int32#2
# 272(1) top = 4
stw 4,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 16
# live double values: 21
# live flags values: 0

# input line 898:   inplace d1 top = m1
# d1#4 top = m1#9
# mem64#20 top = int32#4
# 280(1) top = 6
stw 6,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 15
# live double values: 21
# live flags values: 0

# input line 899:   inplace d2 top = m2
# d2#4 top = m2#9
# mem64#21 top = int32#5
# 288(1) top = 7
stw 7,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 14
# live double values: 21
# live flags values: 0

# input line 900:   inplace d3 top = m3
# d3#4 top = m3#9
# mem64#22 top = int32#7
# 296(1) top = 9
stw 9,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 13
# live double values: 21
# live flags values: 0

# input line 901: 

# input line 902: 

# input line 903:   alpha0 = *(double *) (constants + 24)
# alpha0#3 = *(double *) (constants + 24)
# double#2 = *(double *) (int32#6 + 24)
# 1 = *(double *) (8 + 24)
lfd 1,24(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 28
# live int32 values: 13
# live double values: 22
# live flags values: 0

# input line 904: 

# input line 905:   z3 = d3
# z3#5 = d3#4
# double#18 = mem64#22
# 17 = 296(1)
lfd 17,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 27
# live int32 values: 13
# live double values: 23
# live flags values: 0

# input line 906: 

# input line 907:   z2 = d2
# z2#5 = d2#4
# double#17 = mem64#21
# 16 = 288(1)
lfd 16,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 26
# live int32 values: 13
# live double values: 24
# live flags values: 0

# input line 908: 

# input line 909:   z1 = d1
# z1#5 = d1#4
# double#16 = mem64#20
# 15 = 280(1)
lfd 15,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 25
# live int32 values: 13
# live double values: 25
# live flags values: 0

# input line 910: 

# input line 911:   z0 = d0
# z0#5 = d0#4
# double#15 = mem64#19
# 14 = 272(1)
lfd 14,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 26
# live flags values: 0

# input line 912: 

# input line 913:   z3 -= alpha96
# z3#6 = z3#5 - alpha96
# double#18 = double#18 - double#5
# 17 = 17 - 4
fsub 17,17,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 26
# live flags values: 0

# input line 914: 

# input line 915:   z2 -= alpha64
# z2#6 = z2#5 - alpha64
# double#17 = double#17 - double#4
# 16 = 16 - 3
fsub 16,16,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 26
# live flags values: 0

# input line 916: 

# input line 917:   z1 -= alpha32
# z1#6 = z1#5 - alpha32
# double#16 = double#16 - double#3
# 15 = 15 - 2
fsub 15,15,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 26
# live flags values: 0

# input line 918: 

# input line 919:   z0 -= alpha0
# z0#6 = z0#5 - alpha0#3
# double#2 = double#15 - double#2
# 1 = 14 - 1
fsub 1,14,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 25
# live flags values: 0

# input line 920: 

# input line 921:   h5 += z3
# h5#10 = h5 + z3#6
# double#12 = double#12 + double#18
# 11 = 11 + 17
fadd 11,11,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 24
# live flags values: 0

# input line 922: 

# input line 923:   h3 += z2
# h3#10 = h3 + z2#6
# double#10 = double#10 + double#17
# 9 = 9 + 16
fadd 9,9,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 23
# live flags values: 0

# input line 924: 

# input line 925:   h1 += z1
# h1#10 = h1 + z1#6
# double#8 = double#8 + double#16
# 7 = 7 + 15
fadd 7,7,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 22
# live flags values: 0

# input line 926: 

# input line 927:   h0 += z0
# h0#10 = h0 + z0#6
# double#2 = double#7 + double#2
# 1 = 6 + 1
fadd 1,6,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 21
# live flags values: 0

# input line 928: 

# input line 929:   y1 = h1 + alpha32
# y1#5 = h1#10 + alpha32
# double#16 = double#8 + double#3
# 15 = 7 + 2
fadd 15,7,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 22
# live flags values: 0

# input line 930: 

# input line 931:   y0 = h0 + alpha32
# y0#5 = h0#10 + alpha32
# double#17 = double#2 + double#3
# 16 = 1 + 2
fadd 16,1,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 23
# live flags values: 0

# input line 932: 

# input line 933:   y7 = h7 + alpha130
# y7#5 = h7 + alpha130
# double#7 = double#14 + double#6
# 6 = 13 + 5
fadd 6,13,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 24
# live flags values: 0

# input line 934: 

# input line 935:   y6 = h6 + alpha130
# y6#5 = h6 + alpha130
# double#15 = double#13 + double#6
# 14 = 12 + 5
fadd 14,12,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 25
# live flags values: 0

# input line 936: 

# input line 937:   y5 = h5 + alpha96
# y5#5 = h5#10 + alpha96
# double#18 = double#12 + double#5
# 17 = 11 + 4
fadd 17,11,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 26
# live flags values: 0

# input line 938: 

# input line 939:   y4 = h4 + alpha96
# y4#5 = h4 + alpha96
# double#19 = double#11 + double#5
# 18 = 10 + 4
fadd 18,10,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 27
# live flags values: 0

# input line 940: 

# input line 941:   y3 = h3 + alpha64
# y3#5 = h3#10 + alpha64
# double#20 = double#10 + double#4
# 19 = 9 + 3
fadd 19,9,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 28
# live flags values: 0

# input line 942: 

# input line 943:   y2 = h2 + alpha64
# y2#5 = h2 + alpha64
# double#21 = double#9 + double#4
# 20 = 8 + 3
fadd 20,8,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 944: 

# input line 945:   y1 -= alpha32
# y1#6 = y1#5 - alpha32
# double#16 = double#16 - double#3
# 15 = 15 - 2
fsub 15,15,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 946: 

# input line 947:   y0 -= alpha32
# y0#6 = y0#5 - alpha32
# double#17 = double#17 - double#3
# 16 = 16 - 2
fsub 16,16,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 948: 

# input line 949:   y7 -= alpha130
# y7#6 = y7#5 - alpha130
# double#7 = double#7 - double#6
# 6 = 6 - 5
fsub 6,6,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 950: 

# input line 951:   y6 -= alpha130
# y6#6 = y6#5 - alpha130
# double#15 = double#15 - double#6
# 14 = 14 - 5
fsub 14,14,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 952: 

# input line 953:   y5 -= alpha96
# y5#6 = y5#5 - alpha96
# double#18 = double#18 - double#5
# 17 = 17 - 4
fsub 17,17,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 954: 

# input line 955:   y4 -= alpha96
# y4#6 = y4#5 - alpha96
# double#19 = double#19 - double#5
# 18 = 18 - 4
fsub 18,18,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 956: 

# input line 957:   y3 -= alpha64
# y3#6 = y3#5 - alpha64
# double#22 = double#20 - double#4
# 21 = 19 - 3
fsub 21,19,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 958: 

# input line 959:   y2 -= alpha64
# y2#6 = y2#5 - alpha64
# double#23 = double#21 - double#4
# 22 = 20 - 3
fsub 22,20,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 960: 

# input line 961:   x1 = h1 - y1
# x1#5 = h1#10 - y1#6
# double#20 = double#8 - double#16
# 19 = 7 - 15
fsub 19,7,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 962: 

# input line 963:   x0 = h0 - y0
# x0#7 = h0#10 - y0#6
# double#21 = double#2 - double#17
# 20 = 1 - 16
fsub 20,1,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 964: 

# input line 965:   x7 = h7 - y7
# x7#5 = h7 - y7#6
# double#2 = double#14 - double#7
# 1 = 13 - 6
fsub 1,13,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 966: 

# input line 967:   x6 = h6 - y6
# x6#7 = h6 - y6#6
# double#8 = double#13 - double#15
# 7 = 12 - 14
fsub 7,12,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 968: 

# input line 969:   x5 = h5 - y5
# x5#5 = h5#10 - y5#6
# double#12 = double#12 - double#18
# 11 = 11 - 17
fsub 11,11,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 970: 

# input line 971:   x4 = h4 - y4
# x4#7 = h4 - y4#6
# double#11 = double#11 - double#19
# 10 = 10 - 18
fsub 10,10,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 972: 

# input line 973:   x3 = h3 - y3
# x3#5 = h3#10 - y3#6
# double#13 = double#10 - double#22
# 12 = 9 - 21
fsub 12,9,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 974: 

# input line 975:   x2 = h2 - y2
# x2#7 = h2 - y2#6
# double#14 = double#9 - double#23
# 13 = 8 - 22
fsub 13,8,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 29
# live flags values: 0

# input line 976: 

# input line 977:   x1 += y7 * scale
# x1#6 = x1#5 + y7#6 * scale
# double#9 = double#20 + double#7 * double#1
# 8 = 19 + 6 * 0
fmadd 8,6,0,19
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 28
# live flags values: 0

# input line 978: 

# input line 979:   x0 += y6 * scale
# x0#8 = x0#7 + y6#6 * scale
# double#10 = double#21 + double#15 * double#1
# 9 = 20 + 14 * 0
fmadd 9,14,0,20
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 27
# live flags values: 0

# input line 980: 

# input line 981:   x7 += y5
# x7#6 = x7#5 + y5#6
# double#2 = double#2 + double#18
# 1 = 1 + 17
fadd 1,1,17
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 26
# live flags values: 0

# input line 982: 

# input line 983:   x6 += y4
# x6#8 = x6#7 + y4#6
# double#7 = double#8 + double#19
# 6 = 7 + 18
fadd 6,7,18
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 25
# live flags values: 0

# input line 984: 

# input line 985:   x5 += y3
# x5#6 = x5#5 + y3#6
# double#8 = double#12 + double#22
# 7 = 11 + 21
fadd 7,11,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 24
# live flags values: 0

# input line 986: 

# input line 987:   x4 += y2
# x4#8 = x4#7 + y2#6
# double#11 = double#11 + double#23
# 10 = 10 + 22
fadd 10,10,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 23
# live flags values: 0

# input line 988: 

# input line 989:   x3 += y1
# x3#6 = x3#5 + y1#6
# double#12 = double#13 + double#16
# 11 = 12 + 15
fadd 11,12,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 22
# live flags values: 0

# input line 990: 

# input line 991:   x2 += y0
# x2#8 = x2#7 + y0#6
# double#13 = double#14 + double#17
# 12 = 13 + 16
fadd 12,13,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 21
# live flags values: 0

# input line 992: 

# input line 993:   x0 += x1
# x0#9 = x0#8 + x1#6
# double#14 = double#10 + double#9
# 13 = 9 + 8
fadd 13,9,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 994: 

# input line 995:   x6 += x7
# x6#9 = x6#8 + x7#6
# double#15 = double#7 + double#2
# 14 = 6 + 1
fadd 14,6,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 996: 

# input line 997:   x4 += x5
# x4#9 = x4#8 + x5#6
# double#18 = double#11 + double#8
# 17 = 10 + 7
fadd 17,10,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 998: 

# input line 999:   x2 += x3
# x2#9 = x2#8 + x3#6
# double#16 = double#13 + double#12
# 15 = 12 + 11
fadd 15,12,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 17
# live flags values: 0

# input line 1000: 

# input line 1001:   h7 = r3high * x0
# h7#8 = r3high#2 * x0#9
# double#13 = double#26 * double#14
# 12 = 25 * 13
fmul 12,25,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 17
# live flags values: 0

# input line 1002: 

# input line 1003:   h6 = r3low * x0
# h6#8 = r3low#3 * x0#9
# double#12 = double#24 * double#14
# 11 = 23 * 13
fmul 11,23,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 17
# live flags values: 0

# input line 1004: 

# input line 1005:   h5 = r2high * x0
# h5#11 = r2high#2 * x0#9
# double#11 = double#31 * double#14
# 10 = 30 * 13
fmul 10,30,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1006: 

# input line 1007:   h4 = r2low * x0
# h4#8 = r2low#3 * x0#9
# double#10 = double#30 * double#14
# 9 = 29 * 13
fmul 9,29,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1008: 

# input line 1009:   h3 = r1high * x0
# h3#11 = r1high#2 * x0#9
# double#9 = double#29 * double#14
# 8 = 28 * 13
fmul 8,28,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1010: 

# input line 1011:   h2 = r1low * x0
# h2#8 = r1low#3 * x0#9
# double#8 = double#28 * double#14
# 7 = 27 * 13
fmul 7,27,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 21
# live flags values: 0

# input line 1012: 

# input line 1013:   h1 = r0high * x0
# h1#11 = r0high#2 * x0#9
# double#7 = double#27 * double#14
# 6 = 26 * 13
fmul 6,26,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 22
# live flags values: 0

# input line 1014: 

# input line 1015:   h0 = r0low * x0
# h0#11 = r0low#3 * x0#9
# double#2 = double#25 * double#14
# 1 = 24 * 13
fmul 1,24,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 22
# live flags values: 0

# input line 1016: 

# input line 1017:   h7 += r2high * x2
# h7#9 = h7#8 + r2high#2 * x2#9
# double#13 = double#13 + double#31 * double#16
# 12 = 12 + 30 * 15
fmadd 12,30,15,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 21
# live flags values: 0

# input line 1018: 

# input line 1019:   h6 += r2low * x2
# h6#9 = h6#8 + r2low#3 * x2#9
# double#12 = double#12 + double#30 * double#16
# 11 = 11 + 29 * 15
fmadd 11,29,15,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1020: 

# input line 1021:   h5 += r1high * x2
# h5#12 = h5#11 + r1high#2 * x2#9
# double#11 = double#11 + double#29 * double#16
# 10 = 10 + 28 * 15
fmadd 10,28,15,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1022: 

# input line 1023:   h4 += r1low * x2
# h4#9 = h4#8 + r1low#3 * x2#9
# double#10 = double#10 + double#28 * double#16
# 9 = 9 + 27 * 15
fmadd 9,27,15,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1024: 

# input line 1025:   h3 += r0high * x2
# h3#12 = h3#11 + r0high#2 * x2#9
# double#9 = double#9 + double#27 * double#16
# 8 = 8 + 26 * 15
fmadd 8,26,15,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1026: 

# input line 1027:   h2 += r0low * x2
# h2#9 = h2#8 + r0low#3 * x2#9
# double#8 = double#8 + double#25 * double#16
# 7 = 7 + 24 * 15
fmadd 7,24,15,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1028: 

# input line 1029:   h7 += r1high * x4
# h7#10 = h7#9 + r1high#2 * x4#9
# double#13 = double#13 + double#29 * double#18
# 12 = 12 + 28 * 17
fmadd 12,28,17,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1030: 

# input line 1031:   h6 += r1low * x4
# h6#10 = h6#9 + r1low#3 * x4#9
# double#12 = double#12 + double#28 * double#18
# 11 = 11 + 27 * 17
fmadd 11,27,17,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 24
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1032: 

# input line 1033:   load sr3high
# sr3high#5 = sr3high@stack
# double#17 = mem64#28
# 16 = 344(1)
lfd 16,344(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 23
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1034:   h5 += r0high * x4
# h5#13 = h5#12 + r0high#2 * x4#9
# double#11 = double#11 + double#27 * double#18
# 10 = 10 + 26 * 17
fmadd 10,26,17,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 23
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1035: 

# input line 1036:   load sr3low
# sr3low#5 = sr3low@stack
# double#21 = mem64#27
# 20 = 336(1)
lfd 20,336(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1037:   h4 += r0low * x4
# h4#10 = h4#9 + r0low#3 * x4#9
# double#10 = double#10 + double#25 * double#18
# 9 = 9 + 24 * 17
fmadd 9,24,17,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 20
# live flags values: 0

# input line 1038: 

# input line 1039:   h7 += r0high * x6
# h7 = h7#10 + r0high#2 * x6#9
# double#14 = double#13 + double#27 * double#15
# 13 = 12 + 26 * 14
fmadd 13,26,14,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1040: 

# input line 1041:   h6 += r0low * x6
# h6 = h6#10 + r0low#3 * x6#9
# double#13 = double#12 + double#25 * double#15
# 12 = 11 + 24 * 14
fmadd 12,24,14,11
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1042: 

# input line 1043:   h1 += sr3high * x2
# h1#12 = h1#11 + sr3high#5 * x2#9
# double#7 = double#7 + double#17 * double#16
# 6 = 6 + 16 * 15
fmadd 6,16,15,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1044: 

# input line 1045:   h0 += sr3low * x2
# h0#12 = h0#11 + sr3low#5 * x2#9
# double#2 = double#2 + double#21 * double#16
# 1 = 1 + 20 * 15
fmadd 1,20,15,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 17
# live flags values: 0

# input line 1046: 

# input line 1047:   load sr2high
# sr2high#5 = sr2high@stack
# double#20 = mem64#26
# 19 = 328(1)
lfd 19,328(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1048:   h3 += sr3high * x4
# h3#13 = h3#12 + sr3high#5 * x4#9
# double#9 = double#9 + double#17 * double#18
# 8 = 8 + 16 * 17
fmadd 8,16,17,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1049: 

# input line 1050:   load sr2low
# sr2low#5 = sr2low@stack
# double#19 = mem64#25
# 18 = 320(1)
lfd 18,320(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1051:   h2 += sr3low * x4
# h2#10 = h2#9 + sr3low#5 * x4#9
# double#8 = double#8 + double#21 * double#18
# 7 = 7 + 20 * 17
fmadd 7,20,17,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1052: 

# input line 1053:   h5 += sr3high * x6
# h5 = h5#13 + sr3high#5 * x6#9
# double#12 = double#11 + double#17 * double#15
# 11 = 10 + 16 * 14
fmadd 11,16,14,10
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1054: 

# input line 1055:   load sr1high
# sr1high#5 = sr1high@stack
# double#17 = mem64#24
# 16 = 312(1)
lfd 16,312(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1056:   h4 += sr3low * x6
# h4 = h4#10 + sr3low#5 * x6#9
# double#11 = double#10 + double#21 * double#15
# 10 = 9 + 20 * 14
fmadd 10,20,14,9
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1057: 

# input line 1058:   load sr1low
# sr1low#5 = sr1low@stack
# double#16 = mem64#23
# 15 = 304(1)
lfd 15,304(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1059:   h1 += sr2high * x4
# h1#13 = h1#12 + sr2high#5 * x4#9
# double#7 = double#7 + double#20 * double#18
# 6 = 6 + 19 * 17
fmadd 6,19,17,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1060: 

# input line 1061:   h0 += sr2low * x4
# h0#13 = h0#12 + sr2low#5 * x4#9
# double#2 = double#2 + double#19 * double#18
# 1 = 1 + 18 * 17
fmadd 1,18,17,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1062: 

# input line 1063:   h3 += sr2high * x6
# h3 = h3#13 + sr2high#5 * x6#9
# double#10 = double#9 + double#20 * double#15
# 9 = 8 + 19 * 14
fmadd 9,19,14,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 17
# live flags values: 0

# input line 1064: 

# input line 1065:   h2 += sr2low * x6
# h2 = h2#10 + sr2low#5 * x6#9
# double#9 = double#8 + double#19 * double#15
# 8 = 7 + 18 * 14
fmadd 8,18,14,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 16
# live flags values: 0

# input line 1066: 

# input line 1067:   h1 += sr1high * x6
# h1 = h1#13 + sr1high#5 * x6#9
# double#8 = double#7 + double#17 * double#15
# 7 = 6 + 16 * 14
fmadd 7,16,14,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 15
# live flags values: 0

# input line 1068: 

# input line 1069:   h0 += sr1low * x6
# h0 = h0#13 + sr1low#5 * x6#9
# double#7 = double#2 + double#16 * double#15
# 6 = 1 + 15 * 14
fmadd 6,15,14,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 13
# live flags values: 0

# input line 1070: 

# input line 1071: 

# input line 1072: 

# input line 1073: nomorebytes
.label.nomorebytes:

# input line 1074: 

# input line 1075:   offset0 = *(double *) (constants + 104)
# offset0 = *(double *) (constants + 104)
# double#22 = *(double *) (int32#6 + 104)
# 21 = *(double *) (8 + 104)
lfd 21,104(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 14
# live flags values: 0

# input line 1076:   y7 = h7 + alpha130
# y7#7 = h7 + alpha130
# double#2 = double#14 + double#6
# 1 = 13 + 5
fadd 1,13,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 15
# live flags values: 0

# input line 1077: 

# input line 1078:   offset1 = *(double *) (constants + 112)
# offset1 = *(double *) (constants + 112)
# double#23 = *(double *) (int32#6 + 112)
# 22 = *(double *) (8 + 112)
lfd 22,112(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 16
# live flags values: 0

# input line 1079:   y0 = h0 + alpha32
# y0#7 = h0 + alpha32
# double#17 = double#7 + double#3
# 16 = 6 + 2
fadd 16,6,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 17
# live flags values: 0

# input line 1080: 

# input line 1081:   offset2 = *(double *) (constants + 120)
# offset2 = *(double *) (constants + 120)
# double#24 = *(double *) (int32#6 + 120)
# 23 = *(double *) (8 + 120)
lfd 23,120(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 18
# live flags values: 0

# input line 1082:   y1 = h1 + alpha32
# y1#7 = h1 + alpha32
# double#16 = double#8 + double#3
# 15 = 7 + 2
fadd 15,7,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 19
# live flags values: 0

# input line 1083: 

# input line 1084:   offset3 = *(double *) (constants + 128)
# offset3 = *(double *) (constants + 128)
# double#25 = *(double *) (int32#6 + 128)
# 24 = *(double *) (8 + 128)
lfd 24,128(8)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 20
# live flags values: 0

# input line 1085:   y2 = h2 + alpha64
# y2#7 = h2 + alpha64
# double#21 = double#9 + double#4
# 20 = 8 + 3
fadd 20,8,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1086: 

# input line 1087:   y7 -= alpha130
# y7#8 = y7#7 - alpha130
# double#15 = double#2 - double#6
# 14 = 1 - 5
fsub 14,1,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1088: 

# input line 1089:   y3 = h3 + alpha64
# y3#7 = h3 + alpha64
# double#20 = double#10 + double#4
# 19 = 9 + 3
fadd 19,9,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 22
# live flags values: 0

# input line 1090: 

# input line 1091:   y4 = h4 + alpha96
# y4#7 = h4 + alpha96
# double#19 = double#11 + double#5
# 18 = 10 + 4
fadd 18,10,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 23
# live flags values: 0

# input line 1092: 

# input line 1093:   y5 = h5 + alpha96
# y5#7 = h5 + alpha96
# double#18 = double#12 + double#5
# 17 = 11 + 4
fadd 17,11,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 24
# live flags values: 0

# input line 1094: 

# input line 1095:   x7 = h7 - y7
# x7#7 = h7 - y7#8
# double#2 = double#14 - double#15
# 1 = 13 - 14
fsub 1,13,14
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 24
# live flags values: 0

# input line 1096: 

# input line 1097:   y0 -= alpha32
# y0#8 = y0#7 - alpha32
# double#17 = double#17 - double#3
# 16 = 16 - 2
fsub 16,16,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 24
# live flags values: 0

# input line 1098: 

# input line 1099:   y1 -= alpha32
# y1#8 = y1#7 - alpha32
# double#14 = double#16 - double#3
# 13 = 15 - 2
fsub 13,15,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 23
# live flags values: 0

# input line 1100: 

# input line 1101:   y2 -= alpha64
# y2#8 = y2#7 - alpha64
# double#21 = double#21 - double#4
# 20 = 20 - 3
fsub 20,20,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 23
# live flags values: 0

# input line 1102: 

# input line 1103:   h6 += x7
# h6#11 = h6 + x7#7
# double#2 = double#13 + double#2
# 1 = 12 + 1
fadd 1,12,1
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 22
# live flags values: 0

# input line 1104: 

# input line 1105:   y3 -= alpha64
# y3#8 = y3#7 - alpha64
# double#16 = double#20 - double#4
# 15 = 19 - 3
fsub 15,19,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1106: 

# input line 1107:   y4 -= alpha96
# y4#8 = y4#7 - alpha96
# double#13 = double#19 - double#5
# 12 = 18 - 4
fsub 12,18,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1108: 

# input line 1109:   y5 -= alpha96
# y5#8 = y5#7 - alpha96
# double#4 = double#18 - double#5
# 3 = 17 - 4
fsub 3,17,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 20
# live flags values: 0

# input line 1110: 

# input line 1111:   y6 = h6 + alpha130
# y6#7 = h6#11 + alpha130
# double#3 = double#2 + double#6
# 2 = 1 + 5
fadd 2,1,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1112: 

# input line 1113:   x0 = h0 - y0
# x0#10 = h0 - y0#8
# double#7 = double#7 - double#17
# 6 = 6 - 16
fsub 6,6,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1114: 

# input line 1115:   x1 = h1 - y1
# x1#7 = h1 - y1#8
# double#5 = double#8 - double#14
# 4 = 7 - 13
fsub 4,7,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1116: 

# input line 1117:   x2 = h2 - y2
# x2#10 = h2 - y2#8
# double#18 = double#9 - double#21
# 17 = 8 - 20
fsub 17,8,20
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 21
# live flags values: 0

# input line 1118: 

# input line 1119:   y6 -= alpha130
# y6#8 = y6#7 - alpha130
# double#3 = double#3 - double#6
# 2 = 2 - 5
fsub 2,2,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 20
# live flags values: 0

# input line 1120: 

# input line 1121:   x0 += y7 * scale
# x0#11 = x0#10 + y7#8 * scale
# double#6 = double#7 + double#15 * double#1
# 5 = 6 + 14 * 0
fmadd 5,14,0,6
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 19
# live flags values: 0

# input line 1122: 

# input line 1123:   x3 = h3 - y3
# x3#7 = h3 - y3#8
# double#9 = double#10 - double#16
# 8 = 9 - 15
fsub 8,9,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 19
# live flags values: 0

# input line 1124: 

# input line 1125:   x4 = h4 - y4
# x4#10 = h4 - y4#8
# double#8 = double#11 - double#13
# 7 = 10 - 12
fsub 7,10,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 19
# live flags values: 0

# input line 1126: 

# input line 1127:   x5 = h5 - y5
# x5#7 = h5 - y5#8
# double#7 = double#12 - double#4
# 6 = 11 - 3
fsub 6,11,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 19
# live flags values: 0

# input line 1128: 

# input line 1129:   x6 = h6 - y6
# x6#10 = h6#11 - y6#8
# double#2 = double#2 - double#3
# 1 = 1 - 2
fsub 1,1,2
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 19
# live flags values: 0

# input line 1130: 

# input line 1131:   x2 += y0
# x2#11 = x2#10 + y0#8
# double#10 = double#18 + double#17
# 9 = 17 + 16
fadd 9,17,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 18
# live flags values: 0

# input line 1132: 

# input line 1133:   x3 += y1
# x3#8 = x3#7 + y1#8
# double#9 = double#9 + double#14
# 8 = 8 + 13
fadd 8,8,13
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 17
# live flags values: 0

# input line 1134: 

# input line 1135:   x4 += y2
# x4#11 = x4#10 + y2#8
# double#8 = double#8 + double#21
# 7 = 7 + 20
fadd 7,7,20
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 16
# live flags values: 0

# input line 1136: 

# input line 1137:   x0 += y6 * scale
# x0#12 = x0#11 + y6#8 * scale
# double#3 = double#6 + double#3 * double#1
# 2 = 5 + 2 * 0
fmadd 2,2,0,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 14
# live flags values: 0

# input line 1138: 

# input line 1139:   x5 += y3
# x5#8 = x5#7 + y3#8
# double#6 = double#7 + double#16
# 5 = 6 + 15
fadd 5,6,15
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 13
# live flags values: 0

# input line 1140: 

# input line 1141:   x6 += y4
# x6#11 = x6#10 + y4#8
# double#1 = double#2 + double#13
# 0 = 1 + 12
fadd 0,1,12
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 12
# live flags values: 0

# input line 1142: 

# input line 1143:   x2 += x3
# x2#12 = x2#11 + x3#8
# double#7 = double#10 + double#9
# 6 = 9 + 8
fadd 6,9,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 11
# live flags values: 0

# input line 1144: 

# input line 1145:   x0 += x1
# x0#13 = x0#12 + x1#7
# double#2 = double#3 + double#5
# 1 = 2 + 4
fadd 1,2,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 10
# live flags values: 0

# input line 1146: 

# input line 1147:   x4 += x5
# x4#12 = x4#11 + x5#8
# double#3 = double#8 + double#6
# 2 = 7 + 5
fadd 2,7,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 9
# live flags values: 0

# input line 1148: 

# input line 1149:   x6 += y5
# x6#12 = x6#11 + y5#8
# double#1 = double#1 + double#4
# 0 = 0 + 3
fadd 0,0,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 8
# live flags values: 0

# input line 1150: 

# input line 1151:   x2 += offset1
# x2#13 = x2#12 + offset1
# double#4 = double#7 + double#23
# 3 = 6 + 22
fadd 3,6,22
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 7
# live flags values: 0

# input line 1152:   d1 = x2
# d1#5 = x2#13
# mem64#20 = double#4
# 280(1) = 3
stfd 3,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 12
# live double values: 6
# live flags values: 0

# input line 1153: 

# input line 1154:   x0 += offset0
# x0#14 = x0#13 + offset0
# double#2 = double#2 + double#22
# 1 = 1 + 21
fadd 1,1,21
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 12
# live double values: 5
# live flags values: 0

# input line 1155:   d0 = x0
# d0#5 = x0#14
# mem64#19 = double#2
# 272(1) = 1
stfd 1,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 12
# live double values: 4
# live flags values: 0

# input line 1156: 

# input line 1157:   x4 += offset2
# x4#13 = x4#12 + offset2
# double#2 = double#3 + double#24
# 1 = 2 + 23
fadd 1,2,23
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 12
# live double values: 3
# live flags values: 0

# input line 1158:   d2 = x4
# d2#5 = x4#13
# mem64#21 = double#2
# 288(1) = 1
stfd 1,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 12
# live double values: 2
# live flags values: 0

# input line 1159: 

# input line 1160:   x6 += offset3
# x6#13 = x6#12 + offset3
# double#1 = double#1 + double#25
# 0 = 0 + 24
fadd 0,0,24
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 12
# live double values: 1
# live flags values: 0

# input line 1161:   d3 = x6
# d3#5 = x6#13
# mem64#22 = double#1
# 296(1) = 0
stfd 0,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 12
# live double values: 0
# live flags values: 0

# input line 1162: 

# input line 1163: 

# input line 1164: 

# input line 1165:   f0 = bottom d0
# f0#3 = bottom d0#5
# int32#4 = bottom mem64#19
# 6 = bottom 272(1)
lwz 6,276(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 22
# live int32 values: 13
# live double values: 0
# live flags values: 0

# input line 1166:   g0 = top d0
# g0 = top d0#5
# int32#2 = top mem64#19
# 4 = top 272(1)
lwz 4,272(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 14
# live double values: 0
# live flags values: 0

# input line 1167:   g0 &= 255
# g0#2 = g0 & 255
# int32#5 = int32#2 & 255
# 7 = 4 & 255
andi. 7,4,255
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 14
# live double values: 0
# live flags values: 0

# input line 1168:   f1 = bottom d1
# f1#3 = bottom d1#5
# int32#2 = bottom mem64#20
# 4 = bottom 280(1)
lwz 4,284(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 21
# live int32 values: 15
# live double values: 0
# live flags values: 0

# input line 1169:   g1 = top d1
# g1 = top d1#5
# int32#6 = top mem64#20
# 8 = top 280(1)
lwz 8,280(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 1170:   g1 &= 255
# g1#2 = g1 & 255
# int32#8 = int32#6 & 255
# 10 = 8 & 255
andi. 10,8,255
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 1171:   f2 = bottom d2
# f2#3 = bottom d2#5
# int32#6 = bottom mem64#21
# 8 = bottom 288(1)
lwz 8,292(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 20
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1172:   g2 = top d2
# g2 = top d2#5
# int32#7 = top mem64#21
# 9 = top 288(1)
lwz 9,288(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1173:   g2 &= 255
# g2#2 = g2 & 255
# int32#9 = int32#7 & 255
# 11 = 9 & 255
andi. 11,9,255
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1174:   f3 = bottom d3
# f3#5 = bottom d3#5
# int32#7 = bottom mem64#22
# 9 = bottom 296(1)
lwz 9,300(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 19
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1175:   g3 = top d3
# g3 = top d3#5
# int32#10 = top mem64#22
# 12 = top 296(1)
lwz 12,296(1)
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1176:   g3 &= 255
# g3#2 = g3 & 255
# int32#21 = int32#10 & 255
# 23 = 12 & 255
andi. 23,12,255
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1177:   f4 = 0
# f4 = 0
# int32#10 = 0
# 12 = 0
li 12,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 1178: 

# input line 1179:   carry f1 = f1 + g0
# carry f1#4 = f1#3 + g0#2
# carry int32#5 = int32#2 + int32#5
# carry 7 = 4 + 7
addc 7,4,7
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1180:   carry f2 = f2 + g1 + carry
# carry f2#4 = f2#3 + g1#2 + carry
# carry int32#8 = int32#6 + int32#8 + carry
# carry 10 = 8 + 10 + carry
adde 10,8,10
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1181:   carry f3 = f3 + g2 + carry
# carry f3#6 = f3#5 + g2#2 + carry
# carry int32#9 = int32#7 + int32#9 + carry
# carry 11 = 9 + 11 + carry
adde 11,9,11
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1182:   carry f4 = f4 + g3 + carry
# carry f4#2 = f4 + g3#2 + carry
# carry int32#2 = int32#10 + int32#21 + carry
# carry 4 = 12 + 23 + carry
adde 4,12,23
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1183: 

# input line 1184:   g0 = 5
# g0#3 = 5
# int32#6 = 5
# 8 = 5
li 8,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1185:   g1 = 0
# g1#3 = 0
# int32#7 = 0
# 9 = 0
li 9,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1186:   g2 = 0
# g2#3 = 0
# int32#10 = 0
# 12 = 0
li 12,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1187:   g3 = 0
# g3#3 = 0
# int32#21 = 0
# 23 = 0
li 23,0
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 1188:   g4 = "-4"
# g4 = -4
# int32#22 = -4
# 24 = -4
li 24,-4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 1189: 

# input line 1190:   carry g0 = g0 + f0
# carry g0#4 = g0#3 + f0#3
# carry int32#6 = int32#6 + int32#4
# carry 8 = 8 + 6
addc 8,8,6
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 1191:   carry g1 = g1 + f1 + carry
# carry g1#4 = g1#3 + f1#4 + carry
# carry int32#7 = int32#7 + int32#5 + carry
# carry 9 = 9 + 7 + carry
adde 9,9,7
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 1192:   carry g2 = g2 + f2 + carry
# carry g2#4 = g2#3 + f2#4 + carry
# carry int32#10 = int32#10 + int32#8 + carry
# carry 12 = 12 + 10 + carry
adde 12,12,10
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 1193:   carry g3 = g3 + f3 + carry
# carry g3#4 = g3#3 + f3#6 + carry
# carry int32#21 = int32#21 + int32#9 + carry
# carry 23 = 23 + 11 + carry
adde 23,23,11
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 22
# live double values: 0
# live flags values: 0

# input line 1194:   carry g4 = g4 + f4 + carry
# carry g4#2 = g4 + f4#2 + carry
# carry int32#2 = int32#22 + int32#2 + carry
# carry 4 = 24 + 4 + carry
adde 4,24,4
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 1195: 

# input line 1196:   f = (int32) g4 >> 16
# f = (int32) g4#2 >> 16
# int32#2 = (int32) int32#2 >> 16
# 4 = (int32) 4 >> 16
srawi 4,4,16
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 1197:   f0 &= f
# f0#4 = f0#3 & f
# int32#4 = int32#4 & int32#2
# 6 = 6 & 4
and 6,6,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 1198:   g0 &= ~f
# g0#5 = g0#4 &~ f
# int32#6 = int32#6 &~ int32#2
# 8 = 8 &~ 4
andc 8,8,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 21
# live double values: 0
# live flags values: 0

# input line 1199:   f0 |= g0
# f0#5 = f0#4 | g0#5
# int32#6 = int32#4 | int32#6
# 8 = 6 | 8
or 8,6,8
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1200:   f1 &= f
# f1#5 = f1#4 & f
# int32#4 = int32#5 & int32#2
# 6 = 7 & 4
and 6,7,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1201:   g1 &= ~f
# g1#5 = g1#4 &~ f
# int32#5 = int32#7 &~ int32#2
# 7 = 9 &~ 4
andc 7,9,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 20
# live double values: 0
# live flags values: 0

# input line 1202:   f1 |= g1
# f1#6 = f1#5 | g1#5
# int32#7 = int32#4 | int32#5
# 9 = 6 | 7
or 9,6,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1203:   f2 &= f
# f2#5 = f2#4 & f
# int32#4 = int32#8 & int32#2
# 6 = 10 & 4
and 6,10,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1204:   g2 &= ~f
# g2#5 = g2#4 &~ f
# int32#5 = int32#10 &~ int32#2
# 7 = 12 &~ 4
andc 7,12,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1205:   f2 |= g2
# f2#6 = f2#5 | g2#5
# int32#8 = int32#4 | int32#5
# 10 = 6 | 7
or 10,6,7
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1206:   f3 &= f
# f3#7 = f3#6 & f
# int32#4 = int32#9 & int32#2
# 6 = 11 & 4
and 6,11,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1207:   g3 &= ~f
# g3#5 = g3#4 &~ f
# int32#2 = int32#21 &~ int32#2
# 4 = 23 &~ 4
andc 4,23,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1208:   f3 |= g3
# f3#8 = f3#7 | g3#5
# int32#9 = int32#4 | int32#2
# 11 = 6 | 4
or 11,6,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 1209: 

# input line 1210:   s0 = reverse *(uint32 *) s
# s0 = reverse *(uint32 *) (s + 0)
# int32#4 = reverse *(uint32 *) (int32#3 + 0)
# 6 = reverse *(uint32 *) (5 + 0)
lwbrx 6,0,5
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1211:   s += 4
# s#2 = s + 4
# int32#2 = int32#3 + 4
# 4 = 5 + 4
addi 4,5,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1212:   s1 = reverse *(uint32 *) s
# s1 = reverse *(uint32 *) (s#2 + 0)
# int32#3 = reverse *(uint32 *) (int32#2 + 0)
# 5 = reverse *(uint32 *) (4 + 0)
lwbrx 5,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1213:   s += 4
# s#3 = s#2 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1214:   s2 = reverse *(uint32 *) s
# s2 = reverse *(uint32 *) (s#3 + 0)
# int32#5 = reverse *(uint32 *) (int32#2 + 0)
# 7 = reverse *(uint32 *) (4 + 0)
lwbrx 7,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1215:   s += 4
# s#4 = s#3 + 4
# int32#2 = int32#2 + 4
# 4 = 4 + 4
addi 4,4,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1216:   s3 = reverse *(uint32 *) s
# s3 = reverse *(uint32 *) (s#4 + 0)
# int32#2 = reverse *(uint32 *) (int32#2 + 0)
# 4 = reverse *(uint32 *) (4 + 0)
lwbrx 4,0,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1217: 

# input line 1218:   carry f0 = f0 + s0
# carry f0#6 = f0#5 + s0
# carry int32#4 = int32#6 + int32#4
# carry 6 = 8 + 6
addc 6,8,6
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1219:   carry f1 = f1 + s1 + carry
# carry f1#7 = f1#6 + s1 + carry
# carry int32#3 = int32#7 + int32#3 + carry
# carry 5 = 9 + 5 + carry
adde 5,9,5
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1220:   carry f2 = f2 + s2 + carry
# carry f2#7 = f2#6 + s2 + carry
# carry int32#5 = int32#8 + int32#5 + carry
# carry 7 = 10 + 7 + carry
adde 7,10,7
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 1221:   carry f3 = f3 + s3 + carry
# carry f3#9 = f3#8 + s3 + carry
# carry int32#2 = int32#9 + int32#2 + carry
# carry 4 = 11 + 4 + carry
adde 4,11,4
# live mem32 values: 9
# live flag values: 1
# live mem64 values: 18
# live int32 values: 15
# live double values: 0
# live flags values: 0

# input line 1222: 

# input line 1223:   *(uint32 *) out = reverse f0
# *(uint32 *) (out + 0) = f0#6
# *(uint32 *) (int32#1 + 0) = int32#4
# *(uint32 *) (3 + 0) = 6
stwbrx 6,0,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 14
# live double values: 0
# live flags values: 0

# input line 1224:   out += 4
# out#2 = out + 4
# int32#1 = int32#1 + 4
# 3 = 3 + 4
addi 3,3,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 14
# live double values: 0
# live flags values: 0

# input line 1225:   *(uint32 *) out = reverse f1
# *(uint32 *) (out#2 + 0) = f1#7
# *(uint32 *) (int32#1 + 0) = int32#3
# *(uint32 *) (3 + 0) = 5
stwbrx 5,0,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 0
# live flags values: 0

# input line 1226:   out += 4
# out#3 = out#2 + 4
# int32#1 = int32#1 + 4
# 3 = 3 + 4
addi 3,3,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 0
# live flags values: 0

# input line 1227:   *(uint32 *) out = reverse f2
# *(uint32 *) (out#3 + 0) = f2#7
# *(uint32 *) (int32#1 + 0) = int32#5
# *(uint32 *) (3 + 0) = 7
stwbrx 7,0,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 0
# live flags values: 0

# input line 1228:   out += 4
# out#4 = out#3 + 4
# int32#1 = int32#1 + 4
# 3 = 3 + 4
addi 3,3,4
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 0
# live flags values: 0

# input line 1229:   *(uint32 *) out = reverse f3
# *(uint32 *) (out#4 + 0) = f3#9
# *(uint32 *) (int32#1 + 0) = int32#2
# *(uint32 *) (3 + 0) = 4
stwbrx 4,0,3
# live mem32 values: 9
# live flag values: 0
# live mem64 values: 18
# live int32 values: 10
# live double values: 0
# live flags values: 0

# input line 1230: 

# input line 1231:   load callerint 23
# %caller_r23#2 = %caller_r23@stack
# int32#21 = mem32#1
# 23 = 352(1)
lwz 23,352(1)
# live mem32 values: 8
# live flag values: 0
# live mem64 values: 18
# live int32 values: 11
# live double values: 0
# live flags values: 0

# input line 1232:   load callerint 24
# %caller_r24#2 = %caller_r24@stack
# int32#22 = mem32#2
# 24 = 356(1)
lwz 24,356(1)
# live mem32 values: 7
# live flag values: 0
# live mem64 values: 18
# live int32 values: 12
# live double values: 0
# live flags values: 0

# input line 1233:   load callerint 25
# %caller_r25#2 = %caller_r25@stack
# int32#23 = mem32#3
# 25 = 360(1)
lwz 25,360(1)
# live mem32 values: 6
# live flag values: 0
# live mem64 values: 18
# live int32 values: 13
# live double values: 0
# live flags values: 0

# input line 1234:   load callerint 26
# %caller_r26#2 = %caller_r26@stack
# int32#24 = mem32#4
# 26 = 364(1)
lwz 26,364(1)
# live mem32 values: 5
# live flag values: 0
# live mem64 values: 18
# live int32 values: 14
# live double values: 0
# live flags values: 0

# input line 1235:   load callerint 27
# %caller_r27#2 = %caller_r27@stack
# int32#25 = mem32#5
# 27 = 368(1)
lwz 27,368(1)
# live mem32 values: 4
# live flag values: 0
# live mem64 values: 18
# live int32 values: 15
# live double values: 0
# live flags values: 0

# input line 1236:   load callerint 28
# %caller_r28#2 = %caller_r28@stack
# int32#26 = mem32#6
# 28 = 372(1)
lwz 28,372(1)
# live mem32 values: 3
# live flag values: 0
# live mem64 values: 18
# live int32 values: 16
# live double values: 0
# live flags values: 0

# input line 1237:   load callerint 29
# %caller_r29#2 = %caller_r29@stack
# int32#27 = mem32#7
# 29 = 376(1)
lwz 29,376(1)
# live mem32 values: 2
# live flag values: 0
# live mem64 values: 18
# live int32 values: 17
# live double values: 0
# live flags values: 0

# input line 1238:   load callerint 30
# %caller_r30#2 = %caller_r30@stack
# int32#28 = mem32#8
# 30 = 380(1)
lwz 30,380(1)
# live mem32 values: 1
# live flag values: 0
# live mem64 values: 18
# live int32 values: 18
# live double values: 0
# live flags values: 0

# input line 1239:   load callerint 31
# %caller_r31#2 = %caller_r31@stack
# int32#29 = mem32#9
# 31 = 384(1)
lwz 31,384(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 18
# live int32 values: 19
# live double values: 0
# live flags values: 0

# input line 1240:   load callerfloat 14
# %caller_f14#2 = %caller_f14@stack
# double#12 = mem64#1
# 11 = 128(1)
lfd 11,128(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 17
# live int32 values: 19
# live double values: 1
# live flags values: 0

# input line 1241:   load callerfloat 15
# %caller_f15#2 = %caller_f15@stack
# double#13 = mem64#2
# 12 = 136(1)
lfd 12,136(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 16
# live int32 values: 19
# live double values: 2
# live flags values: 0

# input line 1242:   load callerfloat 16
# %caller_f16#2 = %caller_f16@stack
# double#14 = mem64#3
# 13 = 144(1)
lfd 13,144(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 15
# live int32 values: 19
# live double values: 3
# live flags values: 0

# input line 1243:   load callerfloat 17
# %caller_f17#2 = %caller_f17@stack
# double#15 = mem64#4
# 14 = 152(1)
lfd 14,152(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 14
# live int32 values: 19
# live double values: 4
# live flags values: 0

# input line 1244:   load callerfloat 18
# %caller_f18#2 = %caller_f18@stack
# double#16 = mem64#5
# 15 = 160(1)
lfd 15,160(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 13
# live int32 values: 19
# live double values: 5
# live flags values: 0

# input line 1245:   load callerfloat 19
# %caller_f19#2 = %caller_f19@stack
# double#17 = mem64#6
# 16 = 168(1)
lfd 16,168(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 12
# live int32 values: 19
# live double values: 6
# live flags values: 0

# input line 1246:   load callerfloat 20
# %caller_f20#2 = %caller_f20@stack
# double#18 = mem64#7
# 17 = 176(1)
lfd 17,176(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 11
# live int32 values: 19
# live double values: 7
# live flags values: 0

# input line 1247:   load callerfloat 21
# %caller_f21#2 = %caller_f21@stack
# double#19 = mem64#8
# 18 = 184(1)
lfd 18,184(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 10
# live int32 values: 19
# live double values: 8
# live flags values: 0

# input line 1248:   load callerfloat 22
# %caller_f22#2 = %caller_f22@stack
# double#20 = mem64#9
# 19 = 192(1)
lfd 19,192(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 9
# live int32 values: 19
# live double values: 9
# live flags values: 0

# input line 1249:   load callerfloat 23
# %caller_f23#2 = %caller_f23@stack
# double#21 = mem64#10
# 20 = 200(1)
lfd 20,200(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 8
# live int32 values: 19
# live double values: 10
# live flags values: 0

# input line 1250:   load callerfloat 24
# %caller_f24#2 = %caller_f24@stack
# double#22 = mem64#11
# 21 = 208(1)
lfd 21,208(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 7
# live int32 values: 19
# live double values: 11
# live flags values: 0

# input line 1251:   load callerfloat 25
# %caller_f25#2 = %caller_f25@stack
# double#23 = mem64#12
# 22 = 216(1)
lfd 22,216(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 6
# live int32 values: 19
# live double values: 12
# live flags values: 0

# input line 1252:   load callerfloat 26
# %caller_f26#2 = %caller_f26@stack
# double#24 = mem64#13
# 23 = 224(1)
lfd 23,224(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 5
# live int32 values: 19
# live double values: 13
# live flags values: 0

# input line 1253:   load callerfloat 27
# %caller_f27#2 = %caller_f27@stack
# double#25 = mem64#14
# 24 = 232(1)
lfd 24,232(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 4
# live int32 values: 19
# live double values: 14
# live flags values: 0

# input line 1254:   load callerfloat 28
# %caller_f28#2 = %caller_f28@stack
# double#26 = mem64#15
# 25 = 240(1)
lfd 25,240(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 3
# live int32 values: 19
# live double values: 15
# live flags values: 0

# input line 1255:   load callerfloat 29
# %caller_f29#2 = %caller_f29@stack
# double#27 = mem64#16
# 26 = 248(1)
lfd 26,248(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 2
# live int32 values: 19
# live double values: 16
# live flags values: 0

# input line 1256:   load callerfloat 30
# %caller_f30#2 = %caller_f30@stack
# double#28 = mem64#17
# 27 = 256(1)
lfd 27,256(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 1
# live int32 values: 19
# live double values: 17
# live flags values: 0

# input line 1257:   load callerfloat 31
# %caller_f31#2 = %caller_f31@stack
# double#29 = mem64#18
# 28 = 264(1)
lfd 28,264(1)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 19
# live double values: 18
# live flags values: 0

# input line 1258: 

# input line 1259: 

# input line 1260: leave
addi 1,1,400
blr
