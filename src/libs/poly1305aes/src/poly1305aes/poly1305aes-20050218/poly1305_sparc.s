# poly1305_sparc.s version 20050131
# Daniel J. Bernstein
# Public domain.

# translated by qhasm-sparc version 20050131

# input line 1: # assumes that floating-point mode is round-to-nearest

# input line 2: 

# input line 3: register double scale

# input line 4: register double alpha0

# input line 5: register double alpha32

# input line 6: register double alpha64

# input line 7: register double alpha96

# input line 8: register double alpha130

# input line 9: register double h0

# input line 10: register double h1

# input line 11: register double h2

# input line 12: register double h3

# input line 13: register double h4

# input line 14: register double h5

# input line 15: register double h6

# input line 16: register double h7

# input line 17: 

# input line 18: register double y7

# input line 19: register double y6

# input line 20: register double y1

# input line 21: register double y0

# input line 22: register double y5

# input line 23: register double y4

# input line 24: register double x7

# input line 25: register double x6

# input line 26: register double x1

# input line 27: register double x0

# input line 28: register double y3

# input line 29: register double y2

# input line 30: register double r3low

# input line 31: register double r0low

# input line 32: register double r3high

# input line 33: register double r0high

# input line 34: register double sr1low

# input line 35: register double x5

# input line 36: register double r3lowx0

# input line 37: register double sr1high

# input line 38: register double x4

# input line 39: register double r0lowx6

# input line 40: register double r1low

# input line 41: register double x3

# input line 42: register double r3highx0

# input line 43: register double r1high

# input line 44: register double x2

# input line 45: register double r0highx6

# input line 46: register double sr2low

# input line 47: register double r0lowx0

# input line 48: register double sr2high

# input line 49: register double sr1lowx6

# input line 50: register double r2low

# input line 51: register double r0highx0

# input line 52: register double r2high

# input line 53: register double sr1highx6

# input line 54: register double sr3low

# input line 55: register double r1lowx0

# input line 56: register double sr3high

# input line 57: register double sr2lowx6

# input line 58: register double sr2lowx6

# input line 59: register double r1highx0

# input line 60: register double sr2highx6

# input line 61: register double r2lowx0

# input line 62: register double sr3lowx6

# input line 63: register double r2highx0

# input line 64: register double sr3highx6

# input line 65: register double r1highx4

# input line 66: register double r1lowx4

# input line 67: register double r0highx4

# input line 68: register double r0lowx4

# input line 69: register double sr3highx4

# input line 70: register double sr3lowx4

# input line 71: register double sr2highx4

# input line 72: register double sr2lowx4

# input line 73: register double r0lowx2

# input line 74: register double r0highx2

# input line 75: register double r1lowx2

# input line 76: register double r1highx2

# input line 77: register double r2lowx2

# input line 78: register double r2highx2

# input line 79: register double sr3lowx2

# input line 80: register double sr3highx2

# input line 81: register double z0

# input line 82: register double z1

# input line 83: register double z2

# input line 84: register double z3

# input line 85: 

# input line 86: register int64 out

# input line 87: register int64 r

# input line 88: register int64 s

# input line 89: register int64 m

# input line 90: register int64 l

# input line 91: 

# input line 92: register int64 r0

# input line 93: register int64 r1

# input line 94: register int64 r2

# input line 95: register int64 r3

# input line 96: register int64 r00

# input line 97: register int64 r01

# input line 98: register int64 r02

# input line 99: register int64 r03

# input line 100: register int64 r10

# input line 101: register int64 r11

# input line 102: register int64 r12

# input line 103: register int64 r13

# input line 104: register int64 r20

# input line 105: register int64 r21

# input line 106: register int64 r22

# input line 107: register int64 r23

# input line 108: register int64 r30

# input line 109: register int64 r31

# input line 110: register int64 r32

# input line 111: register int64 r33

# input line 112: register int64 m0

# input line 113: register int64 m1

# input line 114: register int64 m2

# input line 115: register int64 m3

# input line 116: register int64 m00

# input line 117: register int64 m01

# input line 118: register int64 m02

# input line 119: register int64 m03

# input line 120: register int64 m10

# input line 121: register int64 m11

# input line 122: register int64 m12

# input line 123: register int64 m13

# input line 124: register int64 m20

# input line 125: register int64 m21

# input line 126: register int64 m22

# input line 127: register int64 m23

# input line 128: register int64 m30

# input line 129: register int64 m31

# input line 130: register int64 m32

# input line 131: register int64 m33

# input line 132: register int64 constants

# input line 133: register int64 constants_low

# input line 134: register int64 lbelow2

# input line 135: register int64 lbelow3

# input line 136: register int64 lbelow4

# input line 137: register int64 lbelow5

# input line 138: register int64 lbelow6

# input line 139: register int64 lbelow7

# input line 140: register int64 lbelow8

# input line 141: register int64 lbelow9

# input line 142: register int64 lbelow10

# input line 143: register int64 lbelow11

# input line 144: register int64 lbelow12

# input line 145: register int64 lbelow13

# input line 146: register int64 lbelow14

# input line 147: register int64 lbelow15

# input line 148: register double alpham80

# input line 149: register double alpham48

# input line 150: register double alpham16

# input line 151: register double alpha18

# input line 152: register double alpha50

# input line 153: register double alpha82

# input line 154: register double alpha112

# input line 155: register double offset0

# input line 156: register double offset1

# input line 157: register double offset2

# input line 158: register double offset3

# input line 159: temporary mem64 d0

# input line 160: temporary mem64 d1

# input line 161: temporary mem64 d2

# input line 162: temporary mem64 d3

# input line 163: 

# input line 164: enter poly1305_sparc
.section ".text"
.align 32
.global poly1305_sparc
poly1305_sparc:
save %sp,-320,%sp

# input line 165: input out

# input line 166: input r

# input line 167: input s

# input line 168: input m

# input line 169: input l

# input line 170: 

# input line 171:   # block 1: prologue

# input line 172: 

# input line 173:   r00 = *(uchar *) (r + 0)
# r00!%l1 = *(uchar *) (r!%i1 + 0)
ldub [%i1+0],%l1
# live registers: 6 int64, 0 double

# input line 174:   constants = "%hh(poly1305_sparc_constants)" * 1024
# constants!%l0 = %hh(poly1305_sparc_constants) * 1024
sethi %hh(poly1305_sparc_constants),%l0
# live registers: 7 int64, 0 double

# input line 175:   constants_low = "%lm(poly1305_sparc_constants)" * 1024
# constants_low!%l3 = %lm(poly1305_sparc_constants) * 1024
sethi %lm(poly1305_sparc_constants),%l3
# live registers: 8 int64, 0 double

# input line 176: 

# input line 177:   r01 = *(uchar *) (r + 1)
# r01!%l2 = *(uchar *) (r!%i1 + 1)
ldub [%i1+1],%l2
# live registers: 9 int64, 0 double

# input line 178:   constants |= "%hm(poly1305_sparc_constants)"
# constants#2!%l4 = constants!%l0 | %hm(poly1305_sparc_constants)
or %l0,%hm(poly1305_sparc_constants),%l4
# live registers: 9 int64, 0 double

# input line 179:   constants_low |= "%lo(poly1305_sparc_constants)"
# constants_low#2!%l6 = constants_low!%l3 | %lo(poly1305_sparc_constants)
or %l3,%lo(poly1305_sparc_constants),%l6
# live registers: 9 int64, 0 double

# input line 180: 

# input line 181:   r02 = *(uchar *) (r + 2)
# r02!%l3 = *(uchar *) (r!%i1 + 2)
ldub [%i1+2],%l3
# live registers: 10 int64, 0 double

# input line 182:   r0 = 2151
# r0!%l0 = 2151
add %g0,2151,%l0
# live registers: 11 int64, 0 double

# input line 183:   constants <<= 32
# constants#3!%l5 = constants#2!%l4 << 32
sllx %l4,32,%l5
# live registers: 11 int64, 0 double

# input line 184: 

# input line 185:   r03 = *(uchar *) (r + 3)
# r03!%l4 = *(uchar *) (r!%i1 + 3)
ldub [%i1+3],%l4
# live registers: 12 int64, 0 double

# input line 186:   r0 <<= 51
# r0#2!%l0 = r0!%l0 << 51
sllx %l0,51,%l0
# live registers: 12 int64, 0 double

# input line 187:   constants |= constants_low
# constants#4!%o4 = constants#3!%l5 | constants_low#2!%l6
or %l5,%l6,%o4
# live registers: 11 int64, 0 double

# input line 188: 

# input line 189:   r10 = *(uchar *) (r + 4)
# r10!%l5 = *(uchar *) (r!%i1 + 4)
ldub [%i1+4],%l5
# live registers: 12 int64, 0 double

# input line 190:   r01 <<= 8
# r01#2!%l2 = r01!%l2 << 8
sllx %l2,8,%l2
# live registers: 12 int64, 0 double

# input line 191:   r0 += r00
# r0#3!%l0 = r0#2!%l0 + r00!%l1
add %l0,%l1,%l0
# live registers: 11 int64, 0 double

# input line 192: 

# input line 193:   r11 = *(uchar *) (r + 5)
# r11!%l6 = *(uchar *) (r!%i1 + 5)
ldub [%i1+5],%l6
# live registers: 12 int64, 0 double

# input line 194:   r02 <<= 16
# r02#2!%l1 = r02!%l3 << 16
sllx %l3,16,%l1
# live registers: 12 int64, 0 double

# input line 195:   r0 += r01
# r0#4!%l0 = r0#3!%l0 + r01#2!%l2
add %l0,%l2,%l0
# live registers: 11 int64, 0 double

# input line 196: 

# input line 197:   r12 = *(uchar *) (r + 6)
# r12!%l3 = *(uchar *) (r!%i1 + 6)
ldub [%i1+6],%l3
# live registers: 12 int64, 0 double

# input line 198:   r03 <<= 24
# r03#2!%l2 = r03!%l4 << 24
sllx %l4,24,%l2
# live registers: 12 int64, 0 double

# input line 199:   r0 += r02
# r0#5!%l0 = r0#4!%l0 + r02#2!%l1
add %l0,%l1,%l0
# live registers: 11 int64, 0 double

# input line 200: 

# input line 201:   r13 = *(uchar *) (r + 7)
# r13!%l4 = *(uchar *) (r!%i1 + 7)
ldub [%i1+7],%l4
# live registers: 12 int64, 0 double

# input line 202:   r1 = 2215
# r1!%l1 = 2215
add %g0,2215,%l1
# live registers: 13 int64, 0 double

# input line 203:   r0 += r03
# r0#6!%l0 = r0#5!%l0 + r03#2!%l2
add %l0,%l2,%l0
# live registers: 12 int64, 0 double

# input line 204: 

# input line 205:   d0 = r0
# d0!spill0 = r0#6!%l0
stx %l0,[%fp+2023]
# live registers: 11 int64, 0 double

# input line 206:   r1 <<= 51
# r1#2!%l0 = r1!%l1 << 51
sllx %l1,51,%l0
# live registers: 11 int64, 0 double

# input line 207:   r2 = 2279
# r2!%l1 = 2279
add %g0,2279,%l1
# live registers: 12 int64, 0 double

# input line 208: 

# input line 209:   r20 = *(uchar *) (r + 8)
# r20!%l7 = *(uchar *) (r!%i1 + 8)
ldub [%i1+8],%l7
# live registers: 13 int64, 0 double

# input line 210:   r11 <<= 8
# r11#2!%l2 = r11!%l6 << 8
sllx %l6,8,%l2
# live registers: 13 int64, 0 double

# input line 211:   r1 += r10
# r1#3!%l0 = r1#2!%l0 + r10!%l5
add %l0,%l5,%l0
# live registers: 12 int64, 0 double

# input line 212:   

# input line 213:   r21 = *(uchar *) (r + 9)
# r21!%l5 = *(uchar *) (r!%i1 + 9)
ldub [%i1+9],%l5
# live registers: 13 int64, 0 double

# input line 214:   r12 <<= 16
# r12#2!%l3 = r12!%l3 << 16
sllx %l3,16,%l3
# live registers: 13 int64, 0 double

# input line 215:   r1 += r11
# r1#4!%l0 = r1#3!%l0 + r11#2!%l2
add %l0,%l2,%l0
# live registers: 12 int64, 0 double

# input line 216: 

# input line 217:   r22 = *(uchar *) (r + 10)
# r22!%l6 = *(uchar *) (r!%i1 + 10)
ldub [%i1+10],%l6
# live registers: 13 int64, 0 double

# input line 218:   r13 <<= 24
# r13#2!%l2 = r13!%l4 << 24
sllx %l4,24,%l2
# live registers: 13 int64, 0 double

# input line 219:   r1 += r12
# r1#5!%l0 = r1#4!%l0 + r12#2!%l3
add %l0,%l3,%l0
# live registers: 12 int64, 0 double

# input line 220: 

# input line 221:   r23 = *(uchar *) (r + 11)
# r23!%l3 = *(uchar *) (r!%i1 + 11)
ldub [%i1+11],%l3
# live registers: 13 int64, 0 double

# input line 222:   r2 <<= 51
# r2#2!%l1 = r2!%l1 << 51
sllx %l1,51,%l1
# live registers: 13 int64, 0 double

# input line 223:   r1 += r13
# r1#6!%l0 = r1#5!%l0 + r13#2!%l2
add %l0,%l2,%l0
# live registers: 12 int64, 0 double

# input line 224: 

# input line 225:   d1 = r1
# d1!spill8 = r1#6!%l0
stx %l0,[%fp+2015]
# live registers: 11 int64, 0 double

# input line 226:   r21 <<= 8
# r21#2!%l2 = r21!%l5 << 8
sllx %l5,8,%l2
# live registers: 11 int64, 0 double

# input line 227:   r2 += r20
# r2#3!%l0 = r2#2!%l1 + r20!%l7
add %l1,%l7,%l0
# live registers: 10 int64, 0 double

# input line 228: 

# input line 229:   r30 = *(uchar *) (r + 12)
# r30!%l4 = *(uchar *) (r!%i1 + 12)
ldub [%i1+12],%l4
# live registers: 11 int64, 0 double

# input line 230:   r22 <<= 16
# r22#2!%l1 = r22!%l6 << 16
sllx %l6,16,%l1
# live registers: 11 int64, 0 double

# input line 231:   r2 += r21
# r2#4!%l0 = r2#3!%l0 + r21#2!%l2
add %l0,%l2,%l0
# live registers: 10 int64, 0 double

# input line 232: 

# input line 233:   r31 = *(uchar *) (r + 13)
# r31!%l5 = *(uchar *) (r!%i1 + 13)
ldub [%i1+13],%l5
# live registers: 11 int64, 0 double

# input line 234:   r23 <<= 24
# r23#2!%l2 = r23!%l3 << 24
sllx %l3,24,%l2
# live registers: 11 int64, 0 double

# input line 235:   r2 += r22
# r2#5!%l0 = r2#4!%l0 + r22#2!%l1
add %l0,%l1,%l0
# live registers: 10 int64, 0 double

# input line 236: 

# input line 237:   r32 = *(uchar *) (r + 14)
# r32!%l3 = *(uchar *) (r!%i1 + 14)
ldub [%i1+14],%l3
# live registers: 11 int64, 0 double

# input line 238:   r2 += r23
# r2#6!%l0 = r2#5!%l0 + r23#2!%l2
add %l0,%l2,%l0
# live registers: 10 int64, 0 double

# input line 239:   r3 = 2343
# r3!%l1 = 2343
add %g0,2343,%l1
# live registers: 11 int64, 0 double

# input line 240: 

# input line 241:   d2 = r2
# d2!spill16 = r2#6!%l0
stx %l0,[%fp+2007]
# live registers: 10 int64, 0 double

# input line 242:   r3 <<= 51
# r3#2!%l0 = r3!%l1 << 51
sllx %l1,51,%l0
# live registers: 10 int64, 0 double

# input line 243:   alpha32 = *(double *) (constants + 40)
# alpha32!%d4 = *(int64 *) (constants#4!%o4 + 40)
ldd [%o4+40],%d4
# range: alpha32!%d4 is in 2^83 {3,...,3}
# live registers: 10 int64, 1 double

# input line 244:   range alpha32 83 3 3
# live registers: 10 int64, 1 double

# input line 245: 

# input line 246:   r33 = *(uchar *) (r + 15)
# r33!%l6 = *(uchar *) (r!%i1 + 15)
ldub [%i1+15],%l6
# live registers: 10 int64, 1 double

# input line 247:   r31 <<= 8
# r31#2!%l1 = r31!%l5 << 8
sllx %l5,8,%l1
# live registers: 10 int64, 1 double

# input line 248:   r3 += r30
# r3#3!%l0 = r3#2!%l0 + r30!%l4
add %l0,%l4,%l0
# live registers: 9 int64, 1 double

# input line 249: 

# input line 250:   r32 <<= 16
# r32#2!%l2 = r32!%l3 << 16
sllx %l3,16,%l2
# live registers: 9 int64, 1 double

# input line 251:   r3 += r31
# r3#4!%l0 = r3#3!%l0 + r31#2!%l1
add %l0,%l1,%l0
# live registers: 8 int64, 1 double

# input line 252: 

# input line 253:   r33 <<= 24
# r33#2!%l1 = r33!%l6 << 24
sllx %l6,24,%l1
# live registers: 8 int64, 1 double

# input line 254:   r3 += r32
# r3#5!%l0 = r3#4!%l0 + r32#2!%l2
add %l0,%l2,%l0
# live registers: 7 int64, 1 double

# input line 255: 

# input line 256:   r3 += r33
# r3#6!%l0 = r3#5!%l0 + r33#2!%l1
add %l0,%l1,%l0
# live registers: 6 int64, 1 double

# input line 257:   h0 = alpha32 - alpha32
# h0!%d12 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d12
# range: h0!%d12 is in 2^0 {-0xb263960148000,...,0x7a353ee908000}
# live registers: 6 int64, 2 double

# input line 258:   range h0 0 0 0
# live registers: 6 int64, 2 double

# input line 259: 

# input line 260:   d3 = r3
# d3!spill24 = r3#6!%l0
stx %l0,[%fp+1999]
# live registers: 5 int64, 2 double

# input line 261:   h1 = alpha32 - alpha32
# h1!%d14 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d14
# range: h1!%d14 is in 2^16 {-0xbfc44c315400,...,0x12fc8a9a9b400}
# live registers: 5 int64, 3 double

# input line 262:   range h1 16 0 0
# live registers: 5 int64, 3 double

# input line 263: 

# input line 264:   alpha0 = *(double *) (constants + 24)
# alpha0!%d2 = *(int64 *) (constants#4!%o4 + 24)
ldd [%o4+24],%d2
# range: alpha0!%d2 is in 2^51 {3,...,3}
# live registers: 5 int64, 4 double

# input line 265:   range alpha0 51 3 3
# live registers: 5 int64, 4 double

# input line 266:   h2 = alpha32 - alpha32
# h2!%d16 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d16
# range: h2!%d16 is in 2^32 {-0xfedf1d3918000,...,0xa430b1a328000}
# live registers: 5 int64, 5 double

# input line 267:   range h2 32 0 0
# live registers: 5 int64, 5 double

# input line 268: 

# input line 269:   alpha64 = *(double *) (constants + 56)
# alpha64!%d6 = *(int64 *) (constants#4!%o4 + 56)
ldd [%o4+56],%d6
# range: alpha64!%d6 is in 2^115 {3,...,3}
# live registers: 5 int64, 6 double

# input line 270:   range alpha64 115 3 3
# live registers: 5 int64, 6 double

# input line 271:   h3 = alpha32 - alpha32
# h3!%d18 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d18
# range: h3!%d18 is in 2^48 {-0xb8c50e67a400,...,0x123096875bc00}
# live registers: 5 int64, 7 double

# input line 272:   range h3 48 0 0
# live registers: 5 int64, 7 double

# input line 273: 

# input line 274:   alpha18 = *(double *) (constants + 32)
# alpha18!%d32 = *(int64 *) (constants#4!%o4 + 32)
ldd [%o4+32],%d32
# range: alpha18!%d32 is in 2^69 {3,...,3}
# live registers: 5 int64, 8 double

# input line 275:   range alpha18 69 3 3
# live registers: 5 int64, 8 double

# input line 276:   h4 = alpha32 - alpha32
# h4!%d20 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d20
# range: h4!%d20 is in 2^64 {-0x14a28f54d30000,...,0xcd84a94b98000}
# live registers: 5 int64, 9 double

# input line 277:   range h4 64 0 0
# live registers: 5 int64, 9 double

# input line 278: 

# input line 279:   r0low = d0
# r0low!%d0 = d0!spill0
ldd [%fp+2023],%d0
# live registers: 5 int64, 10 double

# input line 280:   h5 = alpha32 - alpha32
# h5!%d22 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d22
# range: h5!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x1167d19c7b800}
# live registers: 5 int64, 11 double

# input line 281:   range h5 80 0 0
# live registers: 5 int64, 11 double

# input line 282: 

# input line 283:   r1low = d1
# r1low!%d8 = d1!spill8
ldd [%fp+2015],%d8
# live registers: 5 int64, 12 double

# input line 284:   h6 = alpha32 - alpha32
# h6!%d24 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d24
# range: h6!%d24 is in 2^96 {-0x1bef3c6fd50000,...,0x1138b739c48000}
# live registers: 5 int64, 13 double

# input line 285:   range h6 96 0 0
# live registers: 5 int64, 13 double

# input line 286: 

# input line 287:   r2low = d2
# r2low!%d10 = d2!spill16
ldd [%fp+2007],%d10
# live registers: 5 int64, 14 double

# input line 288:   h7 = alpha32 - alpha32
# h7!%d26 = alpha32!%d4 - alpha32!%d4
fsubd %d4,%d4,%d26
# range: h7!%d26 is in 2^112 {-0x9f19e1022000,...,0xfa22f1ff7000}
# live registers: 5 int64, 15 double

# input line 289:   range h7 112 0 0
# live registers: 5 int64, 15 double

# input line 290: 

# input line 291:   alpha50 = *(double *) (constants + 48)
# alpha50!%d40 = *(int64 *) (constants#4!%o4 + 48)
ldd [%o4+48],%d40
# range: alpha50!%d40 is in 2^101 {3,...,3}
# live registers: 5 int64, 16 double

# input line 292:   range alpha50 101 3 3
# live registers: 5 int64, 16 double

# input line 293:   r0low -= alpha0
# r0low#2!%d30 = r0low!%d0 - alpha0!%d2
fsubd %d0,%d2,%d30
# range: r0low#2!%d30 is in 2^0 {0,...,0x0fffffff}
# live registers: 5 int64, 16 double

# input line 294:   range r0low 0 0 0x0fffffff
# live registers: 5 int64, 16 double

# input line 295: 

# input line 296:   alpha82 = *(double *) (constants + 64)
# alpha82!%d48 = *(int64 *) (constants#4!%o4 + 64)
ldd [%o4+64],%d48
# range: alpha82!%d48 is in 2^133 {3,...,3}
# live registers: 5 int64, 17 double

# input line 297:   range alpha82 133 3 3
# live registers: 5 int64, 17 double

# input line 298:   r1low -= alpha32
# r1low#2!%d38 = r1low!%d8 - alpha32!%d4
fsubd %d8,%d4,%d38
# range: r1low#2!%d38 is in 2^34 {0,...,0x3ffffff}
# live registers: 5 int64, 17 double

# input line 299:   range r1low 34 0 0x3ffffff
# live registers: 5 int64, 17 double

# input line 300: 

# input line 301:   scale = *(double *) (constants + 96)
# scale!%d0 = *(int64 *) (constants#4!%o4 + 96)
ldd [%o4+96],%d0
# range: scale!%d0 is in 2^-130 {5,...,5}
# live registers: 5 int64, 18 double

# input line 302:   range scale "-130" 5 5
# live registers: 5 int64, 18 double

# input line 303:   r2low -= alpha64
# r2low#2!%d46 = r2low!%d10 - alpha64!%d6
fsubd %d10,%d6,%d46
# range: r2low#2!%d46 is in 2^66 {0,...,0x3ffffff}
# live registers: 5 int64, 18 double

# input line 304:   range r2low 66 0 0x3ffffff
# live registers: 5 int64, 18 double

# input line 305: 

# input line 306:   alpha96 = *(double *) (constants + 72)
# alpha96!%d8 = *(int64 *) (constants#4!%o4 + 72)
ldd [%o4+72],%d8
# range: alpha96!%d8 is in 2^147 {3,...,3}
# live registers: 5 int64, 19 double

# input line 307:   range alpha96 147 3 3
# live registers: 5 int64, 19 double

# input line 308:   r0high = r0low + alpha18
# r0high!%d28 = r0low#2!%d30 + alpha18!%d32
faddd %d30,%d32,%d28
# range: r0high!%d28 is in 2^18 {0x18000000000000,...,0x18000000000400}
# live registers: 5 int64, 20 double

# input line 309: 

# input line 310:   r3low = d3
# r3low!%d10 = d3!spill24
ldd [%fp+1999],%d10
# live registers: 5 int64, 21 double

# input line 311: 

# input line 312:   alpham80 = *(double *) (constants + 0)
# alpham80!%d52 = *(int64 *) (constants#4!%o4 + 0)
ldd [%o4+0],%d52
# range: alpham80!%d52 is in 2^-29 {3,...,3}
# live registers: 5 int64, 22 double

# input line 313:   range alpham80 "-29" 3 3
# live registers: 5 int64, 22 double

# input line 314:   r1high = r1low + alpha50
# r1high!%d36 = r1low#2!%d38 + alpha50!%d40
faddd %d38,%d40,%d36
# range: r1high!%d36 is in 2^50 {0x18000000000000,...,0x18000000000400}
# live registers: 5 int64, 23 double

# input line 315:   sr1low = scale * r1low
# sr1low!%d34 = scale!%d0 * r1low#2!%d38
fmuld %d0,%d38,%d34
# range: sr1low!%d34 is in 2^-96 {0,...,0x13fffffb}
# live registers: 5 int64, 24 double

# input line 316: 

# input line 317:   alpham48 = *(double *) (constants + 8)
# alpham48!%d54 = *(int64 *) (constants#4!%o4 + 8)
ldd [%o4+8],%d54
# range: alpham48!%d54 is in 2^3 {3,...,3}
# live registers: 5 int64, 25 double

# input line 318:   range alpham48 "3" 3 3
# live registers: 5 int64, 25 double

# input line 319:   r2high = r2low + alpha82
# r2high!%d44 = r2low#2!%d46 + alpha82!%d48
faddd %d46,%d48,%d44
# range: r2high!%d44 is in 2^82 {0x18000000000000,...,0x18000000000400}
# live registers: 5 int64, 26 double

# input line 320:   sr2low = scale * r2low
# sr2low!%d42 = scale!%d0 * r2low#2!%d46
fmuld %d0,%d46,%d42
# range: sr2low!%d42 is in 2^-64 {0,...,0x13fffffb}
# live registers: 5 int64, 27 double

# input line 321: 

# input line 322:   exact r0high -= alpha18
# r0high#2!%d32 = r0high!%d28 - alpha18!%d32
fsubd %d28,%d32,%d32
# range: r0high#2!%d32 is in 2^18 {0x0,...,0x400}
# live registers: 5 int64, 26 double

# input line 323:   spill r0high
# r0high@spill!spill24 = r0high#2!%d32
std %d32,[%fp+1999]
# range: r0high@spill!spill24 is in 2^18 {0x0,...,0x400}
# live registers: 5 int64, 26 double

# input line 324: 

# input line 325:   r3low -= alpha96
# r3low#2!%d28 = r3low!%d10 - alpha96!%d8
fsubd %d10,%d8,%d28
# range: r3low#2!%d28 is in 2^98 {0,...,0x3ffffff}
# live registers: 5 int64, 26 double

# input line 326:   range r3low 98 0 0x3ffffff
# live registers: 5 int64, 26 double

# input line 327: 

# input line 328:   exact r1high -= alpha50
# r1high#2!%d40 = r1high!%d36 - alpha50!%d40
fsubd %d36,%d40,%d40
# range: r1high#2!%d40 is in 2^50 {0x0,...,0x400}
# live registers: 5 int64, 25 double

# input line 329:   spill r1high
# r1high@spill!spill56 = r1high#2!%d40
std %d40,[%fp+1967]
# range: r1high@spill!spill56 is in 2^50 {0x0,...,0x400}
# live registers: 5 int64, 25 double

# input line 330: 

# input line 331:   sr1high = sr1low + alpham80
# sr1high!%d36 = sr1low!%d34 + alpham80!%d52
faddd %d34,%d52,%d36
# range: sr1high!%d36 is in 2^-80 {0x18000000000000,...,0x18000000001400}
# live registers: 5 int64, 26 double

# input line 332: 

# input line 333:   alpha112 = *(double *) (constants + 80)
# alpha112!%d58 = *(int64 *) (constants#4!%o4 + 80)
ldd [%o4+80],%d58
# range: alpha112!%d58 is in 2^163 {3,...,3}
# live registers: 5 int64, 27 double

# input line 334:   range alpha112 163 3 3
# live registers: 5 int64, 27 double

# input line 335:   exact r0low -= r0high
# r0low#3!%d30 = r0low#2!%d30 - r0high#2!%d32
fsubd %d30,%d32,%d30
# range: r0low#3!%d30 is in 2^0 {-0x20000,...,0x20000}
# live registers: 5 int64, 26 double

# input line 336: 

# input line 337:   alpham16 = *(double *) (constants + 16)
# alpham16!%d56 = *(int64 *) (constants#4!%o4 + 16)
ldd [%o4+16],%d56
# range: alpham16!%d56 is in 2^35 {3,...,3}
# live registers: 5 int64, 27 double

# input line 338:   range alpham16 35 3 3
# live registers: 5 int64, 27 double

# input line 339:   exact r2high -= alpha82
# r2high#2!%d48 = r2high!%d44 - alpha82!%d48
fsubd %d44,%d48,%d48
# range: r2high#2!%d48 is in 2^82 {0x0,...,0x400}
# live registers: 5 int64, 26 double

# input line 340:   sr3low = scale * r3low
# sr3low!%d50 = scale!%d0 * r3low#2!%d28
fmuld %d0,%d28,%d50
# range: sr3low!%d50 is in 2^-32 {0,...,0x13fffffb}
# live registers: 5 int64, 27 double

# input line 341: 

# input line 342:   alpha130 = *(double *) (constants + 88)
# alpha130!%d10 = *(int64 *) (constants#4!%o4 + 88)
ldd [%o4+88],%d10
# range: alpha130!%d10 is in 2^181 {3,...,3}
# live registers: 5 int64, 28 double

# input line 343:   range alpha130 181 3 3
# live registers: 5 int64, 28 double

# input line 344:   sr2high = sr2low + alpham48
# sr2high!%d44 = sr2low!%d42 + alpham48!%d54
faddd %d42,%d54,%d44
# range: sr2high!%d44 is in 2^-48 {0x18000000000000,...,0x18000000001400}
# live registers: 5 int64, 29 double

# input line 345: 

# input line 346:   exact r1low -= r1high
# r1low#3!%d32 = r1low#2!%d38 - r1high#2!%d40
fsubd %d38,%d40,%d32
# range: r1low#3!%d32 is in 2^34 {-0x8000,...,0x8000}
# live registers: 5 int64, 28 double

# input line 347:   spill r1low
# r1low@spill!spill48 = r1low#3!%d32
std %d32,[%fp+1975]
# range: r1low@spill!spill48 is in 2^34 {-0x8000,...,0x8000}
# live registers: 5 int64, 27 double

# input line 348: 

# input line 349:   exact sr1high -= alpham80
# sr1high#2!%d36 = sr1high!%d36 - alpham80!%d52
fsubd %d36,%d52,%d36
# range: sr1high#2!%d36 is in 2^-80 {0x0,...,0x1400}
# live registers: 5 int64, 26 double

# input line 350:   spill sr1high
# sr1high@spill!spill40 = sr1high#2!%d36
std %d36,[%fp+1983]
# range: sr1high@spill!spill40 is in 2^-80 {0x0,...,0x1400}
# live registers: 5 int64, 26 double

# input line 351: 

# input line 352:   exact r2low -= r2high
# r2low#3!%d32 = r2low#2!%d46 - r2high#2!%d48
fsubd %d46,%d48,%d32
# range: r2low#3!%d32 is in 2^66 {-0x8000,...,0x8000}
# live registers: 5 int64, 26 double

# input line 353:   spill r2low
# r2low@spill!spill80 = r2low#3!%d32
std %d32,[%fp+1943]
# range: r2low@spill!spill80 is in 2^66 {-0x8000,...,0x8000}
# live registers: 5 int64, 25 double

# input line 354: 

# input line 355:   exact sr2high -= alpham48
# sr2high#2!%d38 = sr2high!%d44 - alpham48!%d54
fsubd %d44,%d54,%d38
# range: sr2high#2!%d38 is in 2^-48 {0x0,...,0x1400}
# live registers: 5 int64, 24 double

# input line 356:   spill sr2high
# sr2high@spill!spill72 = sr2high#2!%d38
std %d38,[%fp+1951]
# range: sr2high@spill!spill72 is in 2^-48 {0x0,...,0x1400}
# live registers: 5 int64, 24 double

# input line 357: 

# input line 358:   r3high = r3low + alpha112
# r3high!%d32 = r3low#2!%d28 + alpha112!%d58
faddd %d28,%d58,%d32
# range: r3high!%d32 is in 2^112 {0x18000000000000,...,0x18000000001000}
# live registers: 5 int64, 25 double

# input line 359:   spill r0low
# r0low@spill!spill8 = r0low#3!%d30
std %d30,[%fp+2015]
# range: r0low@spill!spill8 is in 2^0 {-0x20000,...,0x20000}
# live registers: 5 int64, 24 double

# input line 360: 

# input line 361:   exact sr1low -= sr1high
# sr1low#2!%d30 = sr1low!%d34 - sr1high#2!%d36
fsubd %d34,%d36,%d30
# range: sr1low#2!%d30 is in 2^-96 {-0x8000,...,0x8000}
# live registers: 5 int64, 23 double

# input line 362:   spill sr1low
# sr1low@spill!spill32 = sr1low#2!%d30
std %d30,[%fp+1991]
# range: sr1low@spill!spill32 is in 2^-96 {-0x8000,...,0x8000}
# live registers: 5 int64, 22 double

# input line 363: 

# input line 364:   sr3high = sr3low + alpham16
# sr3high!%d34 = sr3low!%d50 + alpham16!%d56
faddd %d50,%d56,%d34
# range: sr3high!%d34 is in 2^-16 {0x18000000000000,...,0x18000000001400}
# live registers: 5 int64, 23 double

# input line 365:   spill r2high
# r2high@spill!spill88 = r2high#2!%d48
std %d48,[%fp+1935]
# range: r2high@spill!spill88 is in 2^82 {0x0,...,0x400}
# live registers: 5 int64, 22 double

# input line 366: 

# input line 367:   exact sr2low -= sr2high
# sr2low#2!%d30 = sr2low!%d42 - sr2high#2!%d38
fsubd %d42,%d38,%d30
# range: sr2low#2!%d30 is in 2^-64 {-0x8000,...,0x8000}
# live registers: 5 int64, 21 double

# input line 368:   spill sr2low
# sr2low@spill!spill64 = sr2low#2!%d30
std %d30,[%fp+1959]
# range: sr2low@spill!spill64 is in 2^-64 {-0x8000,...,0x8000}
# live registers: 5 int64, 20 double

# input line 369: 

# input line 370:   exact r3high -= alpha112
# r3high#2!%d30 = r3high!%d32 - alpha112!%d58
fsubd %d32,%d58,%d30
# range: r3high#2!%d30 is in 2^112 {0x0,...,0x1000}
# live registers: 5 int64, 19 double

# input line 371:   spill r3high
# r3high@spill!spill16 = r3high#2!%d30
std %d30,[%fp+2007]
# range: r3high@spill!spill16 is in 2^112 {0x0,...,0x1000}
# live registers: 5 int64, 19 double

# input line 372: 

# input line 373: 

# input line 374:   exact sr3high -= alpham16
# sr3high#2!%d32 = sr3high!%d34 - alpham16!%d56
fsubd %d34,%d56,%d32
# range: sr3high#2!%d32 is in 2^-16 {0x0,...,0x1400}
# live registers: 5 int64, 18 double

# input line 375:   spill sr3high
# sr3high@spill!spill104 = sr3high#2!%d32
std %d32,[%fp+1919]
# range: sr3high@spill!spill104 is in 2^-16 {0x0,...,0x1400}
# live registers: 5 int64, 18 double

# input line 376: 

# input line 377: 

# input line 378: flags l - 16
# flags l!%i4 - 16
subcc %i4,16,%g0
# live registers: 5 int64, 18 double

# input line 379:   exact r3low -= r3high
# r3low#3!%d28 = r3low#2!%d28 - r3high#2!%d30
fsubd %d28,%d30,%d28
# range: r3low#3!%d28 is in 2^98 {-0x2000,...,0x2000}
# live registers: 5 int64, 17 double

# input line 380:   spill r3low
# r3low@spill!spill0 = r3low#3!%d28
std %d28,[%fp+2023]
# range: r3low@spill!spill0 is in 2^98 {-0x2000,...,0x2000}
# live registers: 5 int64, 16 double

# input line 381: 

# input line 382: 

# input line 383:   exact sr3low -= sr3high
# sr3low#2!%d28 = sr3low!%d50 - sr3high#2!%d32
fsubd %d50,%d32,%d28
# range: sr3low#2!%d28 is in 2^-32 {-0x8000,...,0x8000}
# live registers: 5 int64, 15 double

# input line 384:   spill sr3low
# branch swapped from next instruction
blu,pt %xcc,._addatmost15bytes
# range: sr3low@spill!spill96 is in 2^-32 {-0x8000,...,0x8000}
# live registers: 5 int64, 14 double

# input line 385:   

# input line 386: goto addatmost15bytes if uint64 <
# sr3low@spill!spill96 = sr3low#2!%d28
std %d28,[%fp+1927]

# input line 387: 

# input line 388:   # block 2: initial addition of full chunk

# input line 389: 

# input line 390:   m00 = *(uchar *) (m + 0)
# m00!%l4 = *(uchar *) (m!%i3 + 0)
ldub [%i3+0],%l4
# live registers: 6 int64, 14 double

# input line 391:   m0 = 2151
# m0!%l0 = 2151
add %g0,2151,%l0
# live registers: 7 int64, 14 double

# input line 392: 

# input line 393:   m0 <<= 51
# m0#2!%l0 = m0!%l0 << 51
sllx %l0,51,%l0
# live registers: 7 int64, 14 double

# input line 394:   m1 = 2215
# m1!%l1 = 2215
add %g0,2215,%l1
# live registers: 8 int64, 14 double

# input line 395:   m01 = *(uchar *) (m + 1)
# m01!%l5 = *(uchar *) (m!%i3 + 1)
ldub [%i3+1],%l5
# live registers: 9 int64, 14 double

# input line 396: 

# input line 397:   m1 <<= 51
# m1#2!%l1 = m1!%l1 << 51
sllx %l1,51,%l1
# live registers: 9 int64, 14 double

# input line 398:   m2 = 2279
# m2!%l2 = 2279
add %g0,2279,%l2
# live registers: 10 int64, 14 double

# input line 399:   m02 = *(uchar *) (m + 2)
# m02!%l6 = *(uchar *) (m!%i3 + 2)
ldub [%i3+2],%l6
# live registers: 11 int64, 14 double

# input line 400: 

# input line 401:   m2 <<= 51
# m2#2!%l2 = m2!%l2 << 51
sllx %l2,51,%l2
# live registers: 11 int64, 14 double

# input line 402:   m3 = 2343
# m3!%l3 = 2343
add %g0,2343,%l3
# live registers: 12 int64, 14 double

# input line 403:   m03 = *(uchar *) (m + 3)
# m03!%l7 = *(uchar *) (m!%i3 + 3)
ldub [%i3+3],%l7
# live registers: 13 int64, 14 double

# input line 404: 

# input line 405:   m10 = *(uchar *) (m + 4)
# m10!%o0 = *(uchar *) (m!%i3 + 4)
ldub [%i3+4],%o0
# live registers: 14 int64, 14 double

# input line 406:   m01 <<= 8
# m01#2!%l5 = m01!%l5 << 8
sllx %l5,8,%l5
# live registers: 14 int64, 14 double

# input line 407:   m0 += m00
# m0#3!%l0 = m0#2!%l0 + m00!%l4
add %l0,%l4,%l0
# live registers: 13 int64, 14 double

# input line 408: 

# input line 409:   m11 = *(uchar *) (m + 5)
# m11!%o1 = *(uchar *) (m!%i3 + 5)
ldub [%i3+5],%o1
# live registers: 14 int64, 14 double

# input line 410:   m02 <<= 16
# m02#2!%l4 = m02!%l6 << 16
sllx %l6,16,%l4
# live registers: 14 int64, 14 double

# input line 411:   m0 += m01
# m0#4!%l0 = m0#3!%l0 + m01#2!%l5
add %l0,%l5,%l0
# live registers: 13 int64, 14 double

# input line 412: 

# input line 413:   m12 = *(uchar *) (m + 6)
# m12!%l6 = *(uchar *) (m!%i3 + 6)
ldub [%i3+6],%l6
# live registers: 14 int64, 14 double

# input line 414:   m03 <<= 24
# m03#2!%l5 = m03!%l7 << 24
sllx %l7,24,%l5
# live registers: 14 int64, 14 double

# input line 415:   m0 += m02
# m0#5!%l0 = m0#4!%l0 + m02#2!%l4
add %l0,%l4,%l0
# live registers: 13 int64, 14 double

# input line 416: 

# input line 417:   m13 = *(uchar *) (m + 7)
# m13!%l7 = *(uchar *) (m!%i3 + 7)
ldub [%i3+7],%l7
# live registers: 14 int64, 14 double

# input line 418:   m3 <<= 51
# m3#2!%l3 = m3!%l3 << 51
sllx %l3,51,%l3
# live registers: 14 int64, 14 double

# input line 419:   m0 += m03
# m0#6!%l0 = m0#5!%l0 + m03#2!%l5
add %l0,%l5,%l0
# live registers: 13 int64, 14 double

# input line 420: 

# input line 421:   m20 = *(uchar *) (m + 8)
# m20!%o2 = *(uchar *) (m!%i3 + 8)
ldub [%i3+8],%o2
# live registers: 14 int64, 14 double

# input line 422:   m11 <<= 8
# m11#2!%l4 = m11!%o1 << 8
sllx %o1,8,%l4
# live registers: 14 int64, 14 double

# input line 423:   m1 += m10
# m1#3!%l1 = m1#2!%l1 + m10!%o0
add %l1,%o0,%l1
# live registers: 13 int64, 14 double

# input line 424: 

# input line 425:   m21 = *(uchar *) (m + 9)
# m21!%o0 = *(uchar *) (m!%i3 + 9)
ldub [%i3+9],%o0
# live registers: 14 int64, 14 double

# input line 426:   m12 <<= 16
# m12#2!%l5 = m12!%l6 << 16
sllx %l6,16,%l5
# live registers: 14 int64, 14 double

# input line 427:   m1 += m11
# m1#4!%l1 = m1#3!%l1 + m11#2!%l4
add %l1,%l4,%l1
# live registers: 13 int64, 14 double

# input line 428:   

# input line 429:   m22 = *(uchar *) (m + 10)
# m22!%l6 = *(uchar *) (m!%i3 + 10)
ldub [%i3+10],%l6
# live registers: 14 int64, 14 double

# input line 430:   m13 <<= 24
# m13#2!%l4 = m13!%l7 << 24
sllx %l7,24,%l4
# live registers: 14 int64, 14 double

# input line 431:   m1 += m12
# m1#5!%l1 = m1#4!%l1 + m12#2!%l5
add %l1,%l5,%l1
# live registers: 13 int64, 14 double

# input line 432: 

# input line 433:   m23 = *(uchar *) (m + 11)
# m23!%l7 = *(uchar *) (m!%i3 + 11)
ldub [%i3+11],%l7
# live registers: 14 int64, 14 double

# input line 434:   m1 += m13
# m1#6!%l1 = m1#5!%l1 + m13#2!%l4
add %l1,%l4,%l1
# live registers: 13 int64, 14 double

# input line 435: 

# input line 436:   m30 = *(uchar *) (m + 12)
# m30!%o1 = *(uchar *) (m!%i3 + 12)
ldub [%i3+12],%o1
# live registers: 14 int64, 14 double

# input line 437:   m21 <<= 8
# m21#2!%l4 = m21!%o0 << 8
sllx %o0,8,%l4
# live registers: 14 int64, 14 double

# input line 438:   m2 += m20
# m2#3!%l2 = m2#2!%l2 + m20!%o2
add %l2,%o2,%l2
# live registers: 13 int64, 14 double

# input line 439: 

# input line 440:   m31 = *(uchar *) (m + 13)
# m31!%o0 = *(uchar *) (m!%i3 + 13)
ldub [%i3+13],%o0
# live registers: 14 int64, 14 double

# input line 441:   m22 <<= 16
# m22#2!%l5 = m22!%l6 << 16
sllx %l6,16,%l5
# live registers: 14 int64, 14 double

# input line 442:   m2 += m21
# m2#4!%l2 = m2#3!%l2 + m21#2!%l4
add %l2,%l4,%l2
# live registers: 13 int64, 14 double

# input line 443: 

# input line 444:   m32 = *(uchar *) (m + 14)
# m32!%l6 = *(uchar *) (m!%i3 + 14)
ldub [%i3+14],%l6
# live registers: 14 int64, 14 double

# input line 445:   m23 <<= 24
# m23#2!%l4 = m23!%l7 << 24
sllx %l7,24,%l4
# live registers: 14 int64, 14 double

# input line 446:   m2 += m22
# m2#5!%l2 = m2#4!%l2 + m22#2!%l5
add %l2,%l5,%l2
# live registers: 13 int64, 14 double

# input line 447: 

# input line 448:   m33 = *(uchar *) (m + 15)
# m33!%l5 = *(uchar *) (m!%i3 + 15)
ldub [%i3+15],%l5
# live registers: 14 int64, 14 double

# input line 449:   m2 += m23
# m2#6!%l2 = m2#5!%l2 + m23#2!%l4
add %l2,%l4,%l2
# live registers: 13 int64, 14 double

# input line 450: 

# input line 451:   d0 = m0
# d0#2!spill112 = m0#6!%l0
stx %l0,[%fp+1911]
# live registers: 12 int64, 14 double

# input line 452:   m31 <<= 8
# m31#2!%l4 = m31!%o0 << 8
sllx %o0,8,%l4
# live registers: 12 int64, 14 double

# input line 453:   m3 += m30
# m3#3!%l0 = m3#2!%l3 + m30!%o1
add %l3,%o1,%l0
# live registers: 11 int64, 14 double

# input line 454: 

# input line 455:   d1 = m1
# d1#2!spill120 = m1#6!%l1
stx %l1,[%fp+1903]
# live registers: 10 int64, 14 double

# input line 456:   m32 <<= 16
# m32#2!%l1 = m32!%l6 << 16
sllx %l6,16,%l1
# live registers: 10 int64, 14 double

# input line 457:   m3 += m31
# m3#4!%l0 = m3#3!%l0 + m31#2!%l4
add %l0,%l4,%l0
# live registers: 9 int64, 14 double

# input line 458: 

# input line 459:   d2 = m2
# d2#2!spill128 = m2#6!%l2
stx %l2,[%fp+1895]
# live registers: 8 int64, 14 double

# input line 460:   m33 += 256
# m33#2!%l2 = m33!%l5 + 256
add %l5,256,%l2
# live registers: 8 int64, 14 double

# input line 461: 

# input line 462:   m33 <<= 24
# m33#3!%l2 = m33#2!%l2 << 24
sllx %l2,24,%l2
# live registers: 8 int64, 14 double

# input line 463:   m3 += m32
# m3#5!%l0 = m3#4!%l0 + m32#2!%l1
add %l0,%l1,%l0
# live registers: 7 int64, 14 double

# input line 464:   

# input line 465:   m3 += m33
# m3#6!%l0 = m3#5!%l0 + m33#3!%l2
add %l0,%l2,%l0
# live registers: 6 int64, 14 double

# input line 466:   d3 = m3
# d3#2!spill136 = m3#6!%l0
stx %l0,[%fp+1887]
# live registers: 5 int64, 14 double

# input line 467: 

# input line 468:   m += 16
# m!%i3 = m!%i3 + 16
add %i3,16,%i3
# live registers: 5 int64, 14 double

# input line 469:   l -= 16
# l!%i4 = l!%i4 - 16
sub %i4,16,%i4
# live registers: 5 int64, 14 double

# input line 470:   

# input line 471:   z0 = d0
# z0!%d28 = d0#2!spill112
ldd [%fp+1911],%d28
# live registers: 5 int64, 15 double

# input line 472: 

# input line 473:   z1 = d1
# z1!%d30 = d1#2!spill120
ldd [%fp+1903],%d30
# live registers: 5 int64, 16 double

# input line 474: 

# input line 475:   z2 = d2
# z2!%d32 = d2#2!spill128
ldd [%fp+1895],%d32
# live registers: 5 int64, 17 double

# input line 476: 

# input line 477:   z3 = d3
# z3!%d34 = d3#2!spill136
ldd [%fp+1887],%d34
# live registers: 5 int64, 18 double

# input line 478:   

# input line 479:   z0 -= alpha0
# z0#2!%d2 = z0!%d28 - alpha0!%d2
fsubd %d28,%d2,%d2
# range: z0#2!%d2 is in 2^0 {0,...,0xffffffff}
# live registers: 5 int64, 17 double

# input line 480: 

# input line 481:   z1 -= alpha32
# z1#2!%d28 = z1!%d30 - alpha32!%d4
fsubd %d30,%d4,%d28
# range: z1#2!%d28 is in 2^32 {0,...,0xffffffff}
# live registers: 5 int64, 17 double

# input line 482: 

# input line 483:   z2 -= alpha64
# z2#2!%d30 = z2!%d32 - alpha64!%d6
fsubd %d32,%d6,%d30
# range: z2#2!%d30 is in 2^64 {0,...,0xffffffff}
# live registers: 5 int64, 17 double

# input line 484: 

# input line 485:   z3 -= alpha96
# z3#2!%d32 = z3!%d34 - alpha96!%d8
fsubd %d34,%d8,%d32
# range: z3#2!%d32 is in 2^96 {0,...,0x1ffffffff}
# live registers: 5 int64, 17 double

# input line 486: 

# input line 487:   range z0 0 0 0xffffffff
# live registers: 5 int64, 17 double

# input line 488:   range z1 32 0 0xffffffff
# live registers: 5 int64, 17 double

# input line 489:   range z2 64 0 0xffffffff
# live registers: 5 int64, 17 double

# input line 490:   range z3 96 0 0x1ffffffff
# live registers: 5 int64, 17 double

# input line 491:   

# input line 492: flags l - 16
# flags l!%i4 - 16
subcc %i4,16,%g0
# live registers: 5 int64, 17 double

# input line 493:   exact h0 += z0
# h0#2!%d2 = h0!%d12 + z0#2!%d2
faddd %d12,%d2,%d2
# range: h0#2!%d2 is in 2^0 {-0xb263960148000,...,0x7a354ee907fff}
# live registers: 5 int64, 16 double

# input line 494: 

# input line 495:   exact h1 += z1
# h1#2!%d14 = h1!%d14 + z1#2!%d28
faddd %d14,%d28,%d14
# range: h1#2!%d14 is in 2^16 {-0xbfc44c315400,...,0x22fc8a9a8b400}
# live registers: 5 int64, 15 double

# input line 496: 

# input line 497:   exact h3 += z2
# h3#2!%d18 = h3!%d18 + z2#2!%d30
faddd %d18,%d30,%d18
# range: h3#2!%d18 is in 2^48 {-0xb8c50e67a400,...,0x223096874bc00}
# live registers: 5 int64, 14 double

# input line 498: 

# input line 499:   exact h5 += z3
# branch swapped from next instruction
blu,pt %xcc,._multiplyaddatmost15bytes
# range: h5#2!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x3167d19c6b800}
# live registers: 5 int64, 13 double

# input line 500: 

# input line 501: goto multiplyaddatmost15bytes if uint64 <
# h5#2!%d22 = h5!%d22 + z3#2!%d32
faddd %d22,%d32,%d22

# input line 502: 

# input line 503: multiplyaddatleast16bytes
._multiplyaddatleast16bytes:

# input line 504:   # block 3: main loop

# input line 505: 

# input line 506:   m2 = 2279
# m2#7!%l0 = 2279
add %g0,2279,%l0
# live registers: 6 int64, 13 double

# input line 507:   m20 = *(uchar *) (m + 8)
# m20#2!%l4 = *(uchar *) (m!%i3 + 8)
ldub [%i3+8],%l4
# live registers: 7 int64, 13 double

# input line 508:   y7 = h7 + alpha130
# y7!%d12 = h7!%d26 + alpha130!%d10
faddd %d26,%d10,%d12
# range: y7!%d12 is in 2^130 {0x17ffffd83987bf,...,0x1800003e88bc80}
# live registers: 7 int64, 14 double

# input line 509: 

# input line 510:   m2 <<= 51
# m2#8!%l2 = m2#7!%l0 << 51
sllx %l0,51,%l2
# live registers: 7 int64, 14 double

# input line 511:   m3 = 2343
# m3#7!%l0 = 2343
add %g0,2343,%l0
# live registers: 8 int64, 14 double

# input line 512:   m21 = *(uchar *) (m + 9)
# m21#3!%l5 = *(uchar *) (m!%i3 + 9)
ldub [%i3+9],%l5
# live registers: 9 int64, 14 double

# input line 513:   y6 = h6 + alpha130
# y6!%d28 = h6!%d24 + alpha130!%d10
faddd %d24,%d10,%d28
# range: y6!%d28 is in 2^130 {0x17fffffff90430,...,0x18000000044e2e}
# live registers: 9 int64, 15 double

# input line 514: 

# input line 515:   m3 <<= 51
# m3#8!%l3 = m3#7!%l0 << 51
sllx %l0,51,%l3
# live registers: 9 int64, 15 double

# input line 516:   m0 = 2151
# m0#7!%l0 = 2151
add %g0,2151,%l0
# live registers: 10 int64, 15 double

# input line 517:   m22 = *(uchar *) (m + 10)
# m22#3!%l6 = *(uchar *) (m!%i3 + 10)
ldub [%i3+10],%l6
# live registers: 11 int64, 15 double

# input line 518:   y1 = h1 + alpha32
# y1!%d30 = h1#2!%d14 + alpha32!%d4
faddd %d14,%d4,%d30
# range: y1!%d30 is in 2^32 {0x17ffff403bb3ce,...,0x1800022fc8a9a9}
# live registers: 11 int64, 16 double

# input line 519: 

# input line 520:   m0 <<= 51
# m0#8!%l0 = m0#7!%l0 << 51
sllx %l0,51,%l0
# live registers: 11 int64, 16 double

# input line 521:   m1 = 2215
# m1#7!%l1 = 2215
add %g0,2215,%l1
# live registers: 12 int64, 16 double

# input line 522:   m23 = *(uchar *) (m + 11)
# m23#3!%l7 = *(uchar *) (m!%i3 + 11)
ldub [%i3+11],%l7
# live registers: 13 int64, 16 double

# input line 523:   y0 = h0 + alpha32
# y0!%d32 = h0#2!%d2 + alpha32!%d4
faddd %d2,%d4,%d32
# range: y0!%d32 is in 2^32 {0x17fffffff4d9c6,...,0x1800000007a355}
# live registers: 13 int64, 17 double

# input line 524: 

# input line 525:   m1 <<= 51
# m1#8!%l1 = m1#7!%l1 << 51
sllx %l1,51,%l1
# live registers: 13 int64, 17 double

# input line 526:   m30 = *(uchar *) (m + 12)
# m30#2!%o0 = *(uchar *) (m!%i3 + 12)
ldub [%i3+12],%o0
# live registers: 14 int64, 17 double

# input line 527:   exact y7 -= alpha130
# y7#2!%d12 = y7!%d12 - alpha130!%d10
fsubd %d12,%d10,%d12
# range: y7#2!%d12 is in 2^130 {-0x27c67841,...,0x3e88bc80}
# live registers: 14 int64, 17 double

# input line 528: 

# input line 529:   m21 <<= 8
# m21#4!%l5 = m21#3!%l5 << 8
sllx %l5,8,%l5
# live registers: 14 int64, 17 double

# input line 530:   m2 += m20
# m2#9!%l2 = m2#8!%l2 + m20#2!%l4
add %l2,%l4,%l2
# live registers: 13 int64, 17 double

# input line 531:   m31 = *(uchar *) (m + 13)
# m31#3!%o1 = *(uchar *) (m!%i3 + 13)
ldub [%i3+13],%o1
# live registers: 14 int64, 17 double

# input line 532:   exact y6 -= alpha130
# y6#2!%d28 = y6!%d28 - alpha130!%d10
fsubd %d28,%d10,%d28
# range: y6#2!%d28 is in 2^130 {-0x6fbd0,...,0x44e2e}
# live registers: 14 int64, 17 double

# input line 533: 

# input line 534:   m22 <<= 16
# m22#4!%l4 = m22#3!%l6 << 16
sllx %l6,16,%l4
# live registers: 14 int64, 17 double

# input line 535:   m2 += m21
# m2#10!%l2 = m2#9!%l2 + m21#4!%l5
add %l2,%l5,%l2
# live registers: 13 int64, 17 double

# input line 536:   m32 = *(uchar *) (m + 14)
# m32#3!%l6 = *(uchar *) (m!%i3 + 14)
ldub [%i3+14],%l6
# live registers: 14 int64, 17 double

# input line 537:   exact y1 -= alpha32
# y1#2!%d30 = y1!%d30 - alpha32!%d4
fsubd %d30,%d4,%d30
# range: y1#2!%d30 is in 2^32 {-0xbfc44c32,...,0x22fc8a9a9}
# live registers: 14 int64, 17 double

# input line 538: 

# input line 539:   m23 <<= 24
# m23#4!%l5 = m23#3!%l7 << 24
sllx %l7,24,%l5
# live registers: 14 int64, 17 double

# input line 540:   m2 += m22
# m2#11!%l2 = m2#10!%l2 + m22#4!%l4
add %l2,%l4,%l2
# live registers: 13 int64, 17 double

# input line 541:   m33 = *(uchar *) (m + 15)
# m33#4!%l7 = *(uchar *) (m!%i3 + 15)
ldub [%i3+15],%l7
# live registers: 14 int64, 17 double

# input line 542:   exact y0 -= alpha32
# y0#2!%d32 = y0!%d32 - alpha32!%d4
fsubd %d32,%d4,%d32
# range: y0#2!%d32 is in 2^32 {-0xb263a,...,0x7a355}
# live registers: 14 int64, 17 double

# input line 543: 

# input line 544:   m2 += m23
# m2#12!%l2 = m2#11!%l2 + m23#4!%l5
add %l2,%l5,%l2
# live registers: 13 int64, 17 double

# input line 545:   m00 = *(uchar *) (m + 0)
# m00#2!%l4 = *(uchar *) (m!%i3 + 0)
ldub [%i3+0],%l4
# live registers: 14 int64, 17 double

# input line 546:   y5 = h5 + alpha96
# y5!%d34 = h5#2!%d22 + alpha96!%d8
faddd %d22,%d8,%d34
# range: y5!%d34 is in 2^96 {0x17ffff4e1e4589,...,0x180003167d19c7}
# live registers: 14 int64, 18 double

# input line 547: 

# input line 548:   m31 <<= 8
# m31#4!%o1 = m31#3!%o1 << 8
sllx %o1,8,%o1
# live registers: 14 int64, 18 double

# input line 549:   m3 += m30
# m3#9!%l3 = m3#8!%l3 + m30#2!%o0
add %l3,%o0,%l3
# live registers: 13 int64, 18 double

# input line 550:   m01 = *(uchar *) (m + 1)
# m01#3!%l5 = *(uchar *) (m!%i3 + 1)
ldub [%i3+1],%l5
# live registers: 14 int64, 18 double

# input line 551:   y4 = h4 + alpha96
# y4!%d36 = h4!%d20 + alpha96!%d8
faddd %d20,%d8,%d36
# range: y4!%d36 is in 2^96 {0x17ffffffeb5d70,...,0x180000000cd84b}
# live registers: 14 int64, 19 double

# input line 552: 

# input line 553:   m32 <<= 16
# m32#4!%o2 = m32#3!%l6 << 16
sllx %l6,16,%o2
# live registers: 14 int64, 19 double

# input line 554:   m02 = *(uchar *) (m + 2)
# m02#3!%l6 = *(uchar *) (m!%i3 + 2)
ldub [%i3+2],%l6
# live registers: 15 int64, 19 double

# input line 555:   exact x7 = h7 - y7
# x7!%d26 = h7!%d26 - y7#2!%d12
fsubd %d26,%d12,%d26
# range: x7!%d26 is in 2^112 {-0x20000,...,0x20000}
# live registers: 15 int64, 19 double

# input line 556:   exact y7 *= scale
# y7#3!%d12 = y7#2!%d12 * scale!%d0
fmuld %d12,%d0,%d12
# range: y7#3!%d12 is in 2^0 {-0xc6e05945,...,0x138abae80}
# live registers: 15 int64, 19 double

# input line 557: 

# input line 558:   m33 += 256
# m33#5!%o0 = m33#4!%l7 + 256
add %l7,256,%o0
# live registers: 15 int64, 19 double

# input line 559:   m03 = *(uchar *) (m + 3)
# m03#3!%l7 = *(uchar *) (m!%i3 + 3)
ldub [%i3+3],%l7
# live registers: 16 int64, 19 double

# input line 560:   exact x6 = h6 - y6
# x6!%d38 = h6!%d24 - y6#2!%d28
fsubd %d24,%d28,%d38
# range: x6!%d38 is in 2^96 {-0x200000000,...,0x200000000}
# live registers: 16 int64, 19 double

# input line 561:   exact y6 *= scale
# y6#3!%d24 = y6#2!%d28 * scale!%d0
fmuld %d28,%d0,%d24
# range: y6#3!%d24 is in 2^0 {-0x22eb10,...,0x1586e6}
# live registers: 16 int64, 19 double

# input line 562: 

# input line 563:   m33 <<= 24
# m33#6!%o3 = m33#5!%o0 << 24
sllx %o0,24,%o3
# live registers: 16 int64, 19 double

# input line 564:   m3 += m31
# m3#10!%l3 = m3#9!%l3 + m31#4!%o1
add %l3,%o1,%l3
# live registers: 15 int64, 19 double

# input line 565:   m10 = *(uchar *) (m + 4)
# m10#2!%o0 = *(uchar *) (m!%i3 + 4)
ldub [%i3+4],%o0
# live registers: 16 int64, 19 double

# input line 566:   exact x1 = h1 - y1
# x1!%d28 = h1#2!%d14 - y1#2!%d30
fsubd %d14,%d30,%d28
# range: x1!%d28 is in 2^16 {-0x8000,...,0x8000}
# live registers: 16 int64, 19 double

# input line 567: 

# input line 568:   m01 <<= 8
# m01#4!%l5 = m01#3!%l5 << 8
sllx %l5,8,%l5
# live registers: 16 int64, 19 double

# input line 569:   m3 += m32
# m3#11!%l3 = m3#10!%l3 + m32#4!%o2
add %l3,%o2,%l3
# live registers: 15 int64, 19 double

# input line 570:   m11 = *(uchar *) (m + 5)
# m11#3!%o1 = *(uchar *) (m!%i3 + 5)
ldub [%i3+5],%o1
# live registers: 16 int64, 19 double

# input line 571:   exact x0 = h0 - y0
# x0!%d40 = h0#2!%d2 - y0#2!%d32
fsubd %d2,%d32,%d40
# range: x0!%d40 is in 2^0 {-0x80000000,...,0x80000000}
# live registers: 16 int64, 19 double

# input line 572: 

# input line 573:   m3 += m33
# m3#12!%l3 = m3#11!%l3 + m33#6!%o3
add %l3,%o3,%l3
# live registers: 15 int64, 19 double

# input line 574:   m0 += m00
# m0#9!%l0 = m0#8!%l0 + m00#2!%l4
add %l0,%l4,%l0
# live registers: 14 int64, 19 double

# input line 575:   m12 = *(uchar *) (m + 6)
# m12#3!%o2 = *(uchar *) (m!%i3 + 6)
ldub [%i3+6],%o2
# live registers: 15 int64, 19 double

# input line 576:   exact y5 -= alpha96
# y5#2!%d2 = y5!%d34 - alpha96!%d8
fsubd %d34,%d8,%d2
# range: y5#2!%d2 is in 2^96 {-0xb1e1ba77,...,0x3167d19c7}
# live registers: 15 int64, 19 double

# input line 577: 

# input line 578:   m02 <<= 16
# m02#4!%l4 = m02#3!%l6 << 16
sllx %l6,16,%l4
# live registers: 15 int64, 19 double

# input line 579:   m0 += m01
# m0#10!%l0 = m0#9!%l0 + m01#4!%l5
add %l0,%l5,%l0
# live registers: 14 int64, 19 double

# input line 580:   m13 = *(uchar *) (m + 7)
# m13#3!%l6 = *(uchar *) (m!%i3 + 7)
ldub [%i3+7],%l6
# live registers: 15 int64, 19 double

# input line 581:   exact y4 -= alpha96
# y4#2!%d14 = y4!%d36 - alpha96!%d8
fsubd %d36,%d8,%d14
# range: y4#2!%d14 is in 2^96 {-0x14a290,...,0xcd84b}
# live registers: 15 int64, 19 double

# input line 582: 

# input line 583:   m03 <<= 24
# m03#4!%l5 = m03#3!%l7 << 24
sllx %l7,24,%l5
# live registers: 15 int64, 19 double

# input line 584:   m0 += m02
# m0#11!%l0 = m0#10!%l0 + m02#4!%l4
add %l0,%l4,%l0
# live registers: 14 int64, 19 double

# input line 585:   d2 = m2
# d2#3!spill128 = m2#12!%l2
stx %l2,[%fp+1895]
# live registers: 13 int64, 19 double

# input line 586:   exact x1 += y7
# x1#2!%d28 = x1!%d28 + y7#3!%d12
faddd %d28,%d12,%d28
# range: x1#2!%d28 is in 2^0 {-0x146e05945,...,0x1b8abae80}
# live registers: 13 int64, 18 double

# input line 587:   

# input line 588:   m0 += m03
# m0#12!%l0 = m0#11!%l0 + m03#4!%l5
add %l0,%l5,%l0
# live registers: 12 int64, 18 double

# input line 589:   d3 = m3
# d3#3!spill136 = m3#12!%l3
stx %l3,[%fp+1887]
# live registers: 11 int64, 18 double

# input line 590:   exact x0 += y6
# x0#2!%d34 = x0!%d40 + y6#3!%d24
faddd %d40,%d24,%d34
# range: x0#2!%d34 is in 2^0 {-0x8022eb10,...,0x801586e6}
# live registers: 11 int64, 17 double

# input line 591: 

# input line 592:   m11 <<= 8
# m11#4!%l2 = m11#3!%o1 << 8
sllx %o1,8,%l2
# live registers: 11 int64, 17 double

# input line 593:   m1 += m10
# m1#9!%l1 = m1#8!%l1 + m10#2!%o0
add %l1,%o0,%l1
# live registers: 10 int64, 17 double

# input line 594:   d0 = m0
# d0#3!spill112 = m0#12!%l0
stx %l0,[%fp+1911]
# live registers: 9 int64, 17 double

# input line 595:   exact x7 += y5
# x7#2!%d12 = x7!%d26 + y5#2!%d2
faddd %d26,%d2,%d12
# range: x7#2!%d12 is in 2^96 {-0x2b1e1ba77,...,0x5167d19c7}
# live registers: 9 int64, 17 double

# input line 596: 

# input line 597:   m12 <<= 16
# m12#4!%l3 = m12#3!%o2 << 16
sllx %o2,16,%l3
# live registers: 9 int64, 17 double

# input line 598:   m1 += m11
# m1#10!%l0 = m1#9!%l1 + m11#4!%l2
add %l1,%l2,%l0
# live registers: 8 int64, 17 double

# input line 599:   exact x6 += y4
# x6#2!%d24 = x6!%d38 + y4#2!%d14
faddd %d38,%d14,%d24
# range: x6#2!%d24 is in 2^96 {-0x20014a290,...,0x2000cd84b}
# live registers: 8 int64, 17 double

# input line 600: 

# input line 601:   m13 <<= 24
# m13#4!%l1 = m13#3!%l6 << 24
sllx %l6,24,%l1
# live registers: 8 int64, 17 double

# input line 602:   m1 += m12
# m1#11!%l0 = m1#10!%l0 + m12#4!%l3
add %l0,%l3,%l0
# live registers: 7 int64, 17 double

# input line 603:   y3 = h3 + alpha64
# y3!%d36 = h3#2!%d18 + alpha64!%d6
faddd %d18,%d6,%d36
# range: y3!%d36 is in 2^64 {0x17ffff473af198,...,0x18000223096875}
# live registers: 7 int64, 18 double

# input line 604: 

# input line 605:   m1 += m13
# m1#12!%l0 = m1#11!%l0 + m13#4!%l1
add %l0,%l1,%l0
# live registers: 6 int64, 18 double

# input line 606:   d1 = m1
# d1#3!spill120 = m1#12!%l0
stx %l0,[%fp+1903]
# live registers: 5 int64, 18 double

# input line 607:   y2 = h2 + alpha64
# y2!%d38 = h2!%d16 + alpha64!%d6
faddd %d16,%d6,%d38
# range: y2!%d38 is in 2^64 {0x17fffffff0120e,...,0x180000000a430c}
# live registers: 5 int64, 19 double

# input line 608:   

# input line 609:   exact x0 += x1
# x0#3!%d28 = x0#2!%d34 + x1#2!%d28
faddd %d34,%d28,%d28
# range: x0#3!%d28 is in 2^0 {-0x1c7034455,...,0x238c13566}
# live registers: 5 int64, 18 double

# input line 610: 

# input line 611:   exact x6 += x7
# x6#3!%d26 = x6#2!%d24 + x7#2!%d12
faddd %d24,%d12,%d26
# range: x6#3!%d26 is in 2^96 {-0x4b1f65d07,...,0x71689f212}
# live registers: 5 int64, 17 double

# input line 612: 

# input line 613:   exact y3 -= alpha64
# y3#2!%d12 = y3!%d36 - alpha64!%d6
fsubd %d36,%d6,%d12
# range: y3#2!%d12 is in 2^64 {-0xb8c50e68,...,0x223096875}
# live registers: 5 int64, 17 double

# input line 614:   unspill r3low
# r3low#4!%d34 = r3low@spill!spill0
ldd [%fp+2023],%d34
# range: r3low#4!%d34 is in 2^98 {-0x2000,...,0x2000}
# live registers: 5 int64, 18 double

# input line 615: 

# input line 616:   exact y2 -= alpha64
# y2#2!%d24 = y2!%d38 - alpha64!%d6
fsubd %d38,%d6,%d24
# range: y2#2!%d24 is in 2^64 {-0xfedf2,...,0xa430c}
# live registers: 5 int64, 18 double

# input line 617:   unspill r0low
# r0low#4!%d36 = r0low@spill!spill8
ldd [%fp+2015],%d36
# range: r0low#4!%d36 is in 2^0 {-0x20000,...,0x20000}
# live registers: 5 int64, 19 double

# input line 618: 

# input line 619:   exact x5 = h5 - y5
# x5!%d22 = h5#2!%d22 - y5#2!%d2
fsubd %d22,%d2,%d22
# range: x5!%d22 is in 2^80 {-0x8000,...,0x8000}
# live registers: 5 int64, 18 double

# input line 620:   exact r3lowx0 = r3low * x0
# r3lowx0!%d38 = r3low#4!%d34 * x0#3!%d28
fmuld %d34,%d28,%d38
# range: r3lowx0!%d38 is in 2^98 {-0x471826acc000,...,0x38e0688aa000}
# live registers: 5 int64, 18 double

# input line 621:   unspill r3high
# r3high#3!%d2 = r3high@spill!spill16
ldd [%fp+2007],%d2
# range: r3high#3!%d2 is in 2^112 {0x0,...,0x1000}
# live registers: 5 int64, 19 double

# input line 622: 

# input line 623:   exact x4 = h4 - y4
# x4!%d20 = h4!%d20 - y4#2!%d14
fsubd %d20,%d14,%d20
# range: x4!%d20 is in 2^64 {-0x80000000,...,0x80000000}
# live registers: 5 int64, 18 double

# input line 624:   exact r0lowx6 = r0low * x6
# r0lowx6!%d40 = r0low#4!%d36 * x6#3!%d26
fmuld %d36,%d26,%d40
# range: r0lowx6!%d40 is in 2^96 {-0xe2d13e4240000,...,0x963ecba0e0000}
# live registers: 5 int64, 19 double

# input line 625:   unspill r0high
# r0high#3!%d34 = r0high@spill!spill24
ldd [%fp+1999],%d34
# range: r0high#3!%d34 is in 2^18 {0x0,...,0x400}
# live registers: 5 int64, 20 double

# input line 626: 

# input line 627:   exact x3 = h3 - y3
# x3!%d44 = h3#2!%d18 - y3#2!%d12
fsubd %d18,%d12,%d44
# range: x3!%d44 is in 2^48 {-0x8000,...,0x8000}
# live registers: 5 int64, 20 double

# input line 628:   exact r3highx0 = r3high * x0
# r3highx0!%d46 = r3high#3!%d2 * x0#3!%d28
fmuld %d2,%d28,%d46
# range: r3highx0!%d46 is in 2^112 {-0x1c7034455000,...,0x238c13566000}
# live registers: 5 int64, 20 double

# input line 629:   unspill sr1low
# sr1low#3!%d2 = sr1low@spill!spill32
ldd [%fp+1991],%d2
# range: sr1low#3!%d2 is in 2^-96 {-0x8000,...,0x8000}
# live registers: 5 int64, 21 double

# input line 630: 

# input line 631:   exact x2 = h2 - y2
# x2!%d48 = h2!%d16 - y2#2!%d24
fsubd %d16,%d24,%d48
# range: x2!%d48 is in 2^32 {-0x80000000,...,0x80000000}
# live registers: 5 int64, 21 double

# input line 632:   exact r0highx6 = r0high * x6
# r0highx6!%d50 = r0high#3!%d34 * x6#3!%d26
fmuld %d34,%d26,%d50
# range: r0highx6!%d50 is in 2^114 {-0x12c7d9741c00,...,0x1c5a27c84800}
# live registers: 5 int64, 22 double

# input line 633:   unspill sr1high
# sr1high#3!%d16 = sr1high@spill!spill40
ldd [%fp+1983],%d16
# range: sr1high#3!%d16 is in 2^-80 {0x0,...,0x1400}
# live registers: 5 int64, 23 double

# input line 634: 

# input line 635:   exact x5 += y3
# x5#2!%d14 = x5!%d22 + y3#2!%d12
faddd %d22,%d12,%d14
# range: x5#2!%d14 is in 2^64 {-0x138c50e68,...,0x2a3096875}
# live registers: 5 int64, 22 double

# input line 636:   exact r0lowx0 = r0low * x0
# r0lowx0!%d12 = r0low#4!%d36 * x0#3!%d28
fmuld %d36,%d28,%d12
# range: r0lowx0!%d12 is in 2^0 {-0x471826acc0000,...,0x38e0688aa0000}
# live registers: 5 int64, 23 double

# input line 637:   unspill r1low
# r1low#4!%d42 = r1low@spill!spill48
ldd [%fp+1975],%d42
# range: r1low#4!%d42 is in 2^34 {-0x8000,...,0x8000}
# live registers: 5 int64, 24 double

# input line 638: 

# input line 639:   exact h6 = r3lowx0 + r0lowx6
# h6#2!%d22 = r3lowx0!%d38 + r0lowx6!%d40
faddd %d38,%d40,%d22
# range: h6#2!%d22 is in 2^96 {-0xf49747ed70000,...,0xa476e5c388000}
# live registers: 5 int64, 23 double

# input line 640:   exact sr1lowx6 = sr1low * x6
# sr1lowx6!%d2 = sr1low#3!%d2 * x6#3!%d26
fmuld %d2,%d26,%d2
# range: sr1lowx6!%d2 is in 2^0 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 5 int64, 23 double

# input line 641:   unspill r1high
# r1high#3!%d38 = r1high@spill!spill56
ldd [%fp+1967],%d38
# range: r1high#3!%d38 is in 2^50 {0x0,...,0x400}
# live registers: 5 int64, 24 double

# input line 642: 

# input line 643:   exact x4 += y2
# x4#2!%d18 = x4!%d20 + y2#2!%d24
faddd %d20,%d24,%d18
# range: x4#2!%d18 is in 2^64 {-0x800fedf2,...,0x800a430c}
# live registers: 5 int64, 23 double

# input line 644:   exact r0highx0 = r0high * x0
# r0highx0!%d52 = r0high#3!%d34 * x0#3!%d28
fmuld %d34,%d28,%d52
# range: r0highx0!%d52 is in 2^18 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 5 int64, 24 double

# input line 645:   unspill sr2low
# sr2low#3!%d40 = sr2low@spill!spill64
ldd [%fp+1959],%d40
# range: sr2low#3!%d40 is in 2^-64 {-0x8000,...,0x8000}
# live registers: 5 int64, 25 double

# input line 646: 

# input line 647:   exact h7 = r3highx0 + r0highx6
# h7#2!%d24 = r3highx0!%d46 + r0highx6!%d50
faddd %d46,%d50,%d24
# range: h7#2!%d24 is in 2^112 {-0x678f9a15c000,...,0x94f4b2778000}
# live registers: 5 int64, 24 double

# input line 648:   exact sr1highx6 = sr1high * x6
# sr1highx6!%d54 = sr1high#3!%d16 * x6#3!%d26
fmuld %d16,%d26,%d54
# range: sr1highx6!%d54 is in 2^16 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 5 int64, 24 double

# input line 649:   unspill sr2high
# sr2high#3!%d46 = sr2high@spill!spill72
ldd [%fp+1951],%d46
# range: sr2high#3!%d46 is in 2^-48 {0x0,...,0x1400}
# live registers: 5 int64, 25 double

# input line 650: 

# input line 651:   exact x3 += y1
# x3#2!%d16 = x3!%d44 + y1#2!%d30
faddd %d44,%d30,%d16
# range: x3#2!%d16 is in 2^32 {-0x13fc44c32,...,0x2afc8a9a9}
# live registers: 5 int64, 24 double

# input line 652:   exact r1lowx0 = r1low * x0
# r1lowx0!%d56 = r1low#4!%d42 * x0#3!%d28
fmuld %d42,%d28,%d56
# range: r1lowx0!%d56 is in 2^34 {-0x11c609ab30000,...,0xe381a22a8000}
# live registers: 5 int64, 25 double

# input line 653:   unspill r2low
# r2low#4!%d44 = r2low@spill!spill80
ldd [%fp+1943],%d44
# range: r2low#4!%d44 is in 2^66 {-0x8000,...,0x8000}
# live registers: 5 int64, 26 double

# input line 654: 

# input line 655:   exact h0 = r0lowx0 + sr1lowx6
# h0#3!%d2 = r0lowx0!%d12 + sr1lowx6!%d2
faddd %d12,%d2,%d2
# range: h0#3!%d2 is in 2^0 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 5 int64, 25 double

# input line 656:   exact sr2lowx6 = sr2low * x6
# sr2lowx6!%d58 = sr2low#3!%d40 * x6#3!%d26
fmuld %d40,%d26,%d58
# range: sr2lowx6!%d58 is in 2^32 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 5 int64, 26 double

# input line 657:   unspill r2high
# r2high#3!%d50 = r2high@spill!spill88
ldd [%fp+1935],%d50
# range: r2high#3!%d50 is in 2^82 {0x0,...,0x400}
# live registers: 5 int64, 27 double

# input line 658: 

# input line 659:   exact x2 += y0
# x2#2!%d20 = x2!%d48 + y0#2!%d32
faddd %d48,%d32,%d20
# range: x2#2!%d20 is in 2^32 {-0x800b263a,...,0x8007a355}
# live registers: 5 int64, 26 double

# input line 660:   exact r1highx0 = r1high * x0
# r1highx0!%d60 = r1high#3!%d38 * x0#3!%d28
fmuld %d38,%d28,%d60
# range: r1highx0!%d60 is in 2^50 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 5 int64, 27 double

# input line 661:   unspill sr3low
# sr3low#3!%d48 = sr3low@spill!spill96
ldd [%fp+1927],%d48
# range: sr3low#3!%d48 is in 2^-32 {-0x8000,...,0x8000}
# live registers: 5 int64, 28 double

# input line 662: 

# input line 663:   exact h1 = r0highx0 + sr1highx6
# h1#3!%d12 = r0highx0!%d52 + sr1highx6!%d54
faddd %d52,%d54,%d12
# range: h1#3!%d12 is in 2^16 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 5 int64, 27 double

# input line 664:   exact sr2highx6 = sr2high * x6
# sr2highx6!%d54 = sr2high#3!%d46 * x6#3!%d26
fmuld %d46,%d26,%d54
# range: sr2highx6!%d54 is in 2^48 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 5 int64, 28 double

# input line 665:   unspill sr3high
# sr3high#3!%d52 = sr3high@spill!spill104
ldd [%fp+1919],%d52
# range: sr3high#3!%d52 is in 2^-16 {0x0,...,0x1400}
# live registers: 5 int64, 29 double

# input line 666: 

# input line 667:   exact x4 += x5
# x4#3!%d30 = x4#2!%d18 + x5#2!%d14
faddd %d18,%d14,%d30
# range: x4#3!%d30 is in 2^64 {-0x1b8d4fc5a,...,0x32313ab81}
# live registers: 5 int64, 28 double

# input line 668:   exact r2lowx0 = r2low * x0
# r2lowx0!%d18 = r2low#4!%d44 * x0#3!%d28
fmuld %d44,%d28,%d18
# range: r2lowx0!%d18 is in 2^66 {-0x11c609ab30000,...,0xe381a22a8000}
# live registers: 5 int64, 29 double

# input line 669:   z2 = d2
# z2#3!%d62 = d2#3!spill128
ldd [%fp+1895],%d62
# live registers: 5 int64, 30 double

# input line 670: 

# input line 671:   exact h2 = r1lowx0 + sr2lowx6
# h2#2!%d14 = r1lowx0!%d56 + sr2lowx6!%d58
faddd %d56,%d58,%d14
# range: h2#2!%d14 is in 2^32 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 5 int64, 29 double

# input line 672:   exact sr3lowx6 = sr3low * x6
# sr3lowx6!%d56 = sr3low#3!%d48 * x6#3!%d26
fmuld %d48,%d26,%d56
# range: sr3lowx6!%d56 is in 2^64 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 5 int64, 30 double

# input line 673: 

# input line 674:   exact x2 += x3
# x2#3!%d32 = x2#2!%d20 + x3#2!%d16
faddd %d20,%d16,%d32
# range: x2#3!%d32 is in 2^32 {-0x1bfcf726c,...,0x32fd04cfe}
# live registers: 5 int64, 29 double

# input line 675:   exact r2highx0 = r2high * x0
# r2highx0!%d20 = r2high#3!%d50 * x0#3!%d28
fmuld %d50,%d28,%d20
# range: r2highx0!%d20 is in 2^82 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 5 int64, 29 double

# input line 676:   z3 = d3
# z3#3!%d58 = d3#3!spill136
ldd [%fp+1887],%d58
# live registers: 5 int64, 30 double

# input line 677: 

# input line 678:   exact h3 = r1highx0 + sr2highx6
# h3#3!%d16 = r1highx0!%d60 + sr2highx6!%d54
faddd %d60,%d54,%d16
# range: h3#3!%d16 is in 2^48 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 5 int64, 29 double

# input line 679:   exact sr3highx6 = sr3high * x6
# sr3highx6!%d26 = sr3high#3!%d52 * x6#3!%d26
fmuld %d52,%d26,%d26
# range: sr3highx6!%d26 is in 2^80 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 5 int64, 29 double

# input line 680: 

# input line 681:   exact r1highx4 = r1high * x4
# r1highx4!%d28 = r1high#3!%d38 * x4#3!%d30
fmuld %d38,%d30,%d28
# range: r1highx4!%d28 is in 2^114 {-0x6e353f16800,...,0xc8c4eae0400}
# live registers: 5 int64, 30 double

# input line 682:   z2 -= alpha64
# z2#4!%d60 = z2#3!%d62 - alpha64!%d6
fsubd %d62,%d6,%d60
# range: z2#4!%d60 is in 2^64 {0,...,0xffffffff}
# live registers: 5 int64, 30 double

# input line 683: 

# input line 684:   exact h4 = r2lowx0 + sr3lowx6
# h4#2!%d18 = r2lowx0!%d18 + sr3lowx6!%d56
faddd %d18,%d56,%d18
# range: h4#2!%d18 is in 2^64 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 5 int64, 29 double

# input line 685:   exact r1lowx4 = r1low * x4
# r1lowx4!%d54 = r1low#4!%d42 * x4#3!%d30
fmuld %d42,%d30,%d54
# range: r1lowx4!%d54 is in 2^98 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 5 int64, 30 double

# input line 686: 

# input line 687:   exact r0highx4 = r0high * x4
# r0highx4!%d56 = r0high#3!%d34 * x4#3!%d30
fmuld %d34,%d30,%d56
# range: r0highx4!%d56 is in 2^82 {-0x6e353f16800,...,0xc8c4eae0400}
# live registers: 5 int64, 31 double

# input line 688:   z3 -= alpha96
# z3#4!%d62 = z3#3!%d58 - alpha96!%d8
fsubd %d58,%d8,%d62
# range: z3#4!%d62 is in 2^96 {0,...,0x1ffffffff}
# live registers: 5 int64, 31 double

# input line 689: 

# input line 690:   exact h5 = r2highx0 + sr3highx6
# h5#3!%d20 = r2highx0!%d20 + sr3highx6!%d26
faddd %d20,%d26,%d20
# range: h5#3!%d20 is in 2^80 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 5 int64, 30 double

# input line 691:   exact r0lowx4 = r0low * x4
# r0lowx4!%d58 = r0low#4!%d36 * x4#3!%d30
fmuld %d36,%d30,%d58
# range: r0lowx4!%d58 is in 2^64 {-0x6462757020000,...,0x371a9f8b40000}
# live registers: 5 int64, 31 double

# input line 692: 

# input line 693:   exact h7 += r1highx4
# h7#3!%d26 = h7#2!%d24 + r1highx4!%d28
faddd %d24,%d28,%d26
# range: h7#3!%d26 is in 2^112 {-0x831ce9db6000,...,0xc725ed2f9000}
# live registers: 5 int64, 30 double

# input line 694:   exact sr3highx4 = sr3high * x4
# sr3highx4!%d28 = sr3high#3!%d52 * x4#3!%d30
fmuld %d52,%d30,%d28
# range: sr3highx4!%d28 is in 2^48 {-0x2270a3b70800,...,0x3ebd89661400}
# live registers: 5 int64, 31 double

# input line 695: 

# input line 696:   exact h6 += r1lowx4
# h6#3!%d24 = h6#2!%d22 + r1lowx4!%d54
faddd %d22,%d54,%d24
# range: h6#3!%d24 is in 2^96 {-0x158f9bd5d90000,...,0xdb91854ec8000}
# live registers: 5 int64, 30 double

# input line 697:   exact sr3lowx4 = sr3low * x4
# sr3lowx4!%d54 = sr3low#3!%d48 * x4#3!%d30
fmuld %d48,%d30,%d54
# range: sr3lowx4!%d54 is in 2^32 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 5 int64, 31 double

# input line 698: 

# input line 699:   exact h5 += r0highx4
# h5#4!%d22 = h5#3!%d20 + r0highx4!%d56
faddd %d20,%d56,%d22
# range: h5#4!%d22 is in 2^80 {-0x95e4c34f7c00,...,0xe38014f7d800}
# live registers: 5 int64, 30 double

# input line 700:   exact sr2highx4 = sr2high * x4
# sr2highx4!%d46 = sr2high#3!%d46 * x4#3!%d30
fmuld %d46,%d30,%d46
# range: sr2highx4!%d46 is in 2^16 {-0x2270a3b70800,...,0x3ebd89661400}
# live registers: 5 int64, 30 double

# input line 701: 

# input line 702:   exact h4 += r0lowx4
# h4#3!%d20 = h4#2!%d18 + r0lowx4!%d58
faddd %d18,%d58,%d20
# range: h4#3!%d20 is in 2^64 {-0xe42eebad70000,...,0x958abafe18000}
# live registers: 5 int64, 29 double

# input line 703:   exact sr2lowx4 = sr2low * x4
# sr2lowx4!%d30 = sr2low#3!%d40 * x4#3!%d30
fmuld %d40,%d30,%d30
# range: sr2lowx4!%d30 is in 2^0 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 5 int64, 28 double

# input line 704: 

# input line 705:   exact h3 += sr3highx4
# h3#4!%d18 = h3#3!%d16 + sr3highx4!%d28
faddd %d16,%d28,%d18
# range: h3#4!%d18 is in 2^48 {-0x9cc81740e400,...,0xf00c63a5dc00}
# live registers: 5 int64, 27 double

# input line 706:   exact r0lowx2 = r0low * x2
# r0lowx2!%d28 = r0low#4!%d36 * x2#3!%d32
fmuld %d36,%d32,%d28
# range: r0lowx2!%d28 is in 2^32 {-0x65fa099fc0000,...,0x37f9ee4d80000}
# live registers: 5 int64, 27 double

# input line 707: 

# input line 708:   exact h2 += sr3lowx4
# h2#3!%d16 = h2#2!%d14 + sr3lowx4!%d54
faddd %d14,%d54,%d16
# range: h2#3!%d16 is in 2^32 {-0x98e5139958000,...,0x6c36c355a8000}
# live registers: 5 int64, 26 double

# input line 709:   exact r0highx2 = r0high * x2
# r0highx2!%d34 = r0high#3!%d34 * x2#3!%d32
fmuld %d34,%d32,%d34
# range: r0highx2!%d34 is in 2^50 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 5 int64, 26 double

# input line 710: 

# input line 711:   exact h1 += sr2highx4
# h1#4!%d14 = h1#3!%d12 + sr2highx4!%d46
faddd %d12,%d46,%d14
# range: h1#4!%d14 is in 2^16 {-0x9cc81740e400,...,0xf00c63a5dc00}
# live registers: 5 int64, 25 double

# input line 712:   exact r1lowx2 = r1low * x2
# r1lowx2!%d36 = r1low#4!%d42 * x2#3!%d32
fmuld %d42,%d32,%d36
# range: r1lowx2!%d36 is in 2^66 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 5 int64, 25 double

# input line 713: 

# input line 714:   exact h0 += sr2lowx4
# h0#4!%d12 = h0#3!%d2 + sr2lowx4!%d30
faddd %d2,%d30,%d12
# range: h0#4!%d12 is in 2^0 {-0x98e5139958000,...,0x6c36c355a8000}
# live registers: 5 int64, 24 double

# input line 715:   exact r1highx2 = r1high * x2
# r1highx2!%d2 = r1high#3!%d38 * x2#3!%d32
fmuld %d38,%d32,%d2
# range: r1highx2!%d2 is in 2^82 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 5 int64, 24 double

# input line 716: 

# input line 717:   exact h2 += r0lowx2
# h2!%d16 = h2#3!%d16 + r0lowx2!%d28
faddd %d16,%d28,%d16
# range: h2!%d16 is in 2^32 {-0xfedf1d3918000,...,0xa430b1a328000}
# live registers: 5 int64, 23 double

# input line 718:   exact r2lowx2 = r2low * x2
# r2lowx2!%d28 = r2low#4!%d44 * x2#3!%d32
fmuld %d44,%d32,%d28
# range: r2lowx2!%d28 is in 2^98 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 5 int64, 23 double

# input line 719: 

# input line 720:   exact h3 += r0highx2
# h3#5!%d18 = h3#4!%d18 + r0highx2!%d34
faddd %d18,%d34,%d18
# range: h3#5!%d18 is in 2^48 {-0xb8c50e67a400,...,0x123096875bc00}
# live registers: 5 int64, 22 double

# input line 721:   exact r2highx2 = r2high * x2
# r2highx2!%d30 = r2high#3!%d50 * x2#3!%d32
fmuld %d50,%d32,%d30
# range: r2highx2!%d30 is in 2^114 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 5 int64, 22 double

# input line 722: 

# input line 723:   exact h4 += r1lowx2
# h4!%d20 = h4#3!%d20 + r1lowx2!%d36
faddd %d20,%d36,%d20
# range: h4!%d20 is in 2^64 {-0x14a28f54d30000,...,0xcd84a94b98000}
# live registers: 5 int64, 21 double

# input line 724:   exact sr3lowx2 = sr3low * x2
# sr3lowx2!%d34 = sr3low#3!%d48 * x2#3!%d32
fmuld %d48,%d32,%d34
# range: sr3lowx2!%d34 is in 2^0 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 5 int64, 21 double

# input line 725: 

# input line 726:   exact h5 += r1highx2
# h5#5!%d22 = h5#4!%d22 + r1highx2!%d2
faddd %d22,%d2,%d22
# range: h5#5!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x1167d19c7b800}
# live registers: 5 int64, 20 double

# input line 727:   exact sr3highx2 = sr3high * x2
# sr3highx2!%d32 = sr3high#3!%d52 * x2#3!%d32
fmuld %d52,%d32,%d32
# range: sr3highx2!%d32 is in 2^16 {-0x22fc34f07000,...,0x3fbc4603d800}
# live registers: 5 int64, 19 double

# input line 728:   alpha0 = *(double *) (constants + 24)
# alpha0#2!%d2 = *(int64 *) (constants#4!%o4 + 24)
ldd [%o4+24],%d2
# range: alpha0#2!%d2 is in 2^51 {3,...,3}
# live registers: 5 int64, 20 double

# input line 729:   range alpha0 51 3 3
# live registers: 5 int64, 20 double

# input line 730: 

# input line 731:   m += 16
# m!%i3 = m!%i3 + 16
add %i3,16,%i3
# live registers: 5 int64, 20 double

# input line 732:   exact h6 += r2lowx2
# h6!%d24 = h6#3!%d24 + r2lowx2!%d28
faddd %d24,%d28,%d24
# range: h6!%d24 is in 2^96 {-0x1bef3c6fd50000,...,0x1138b739c48000}
# live registers: 5 int64, 19 double

# input line 733: 

# input line 734:   l -= 16
# l!%i4 = l!%i4 - 16
sub %i4,16,%i4
# live registers: 5 int64, 19 double

# input line 735:   exact h7 += r2highx2
# h7!%d26 = h7#3!%d26 + r2highx2!%d30
faddd %d26,%d30,%d26
# range: h7!%d26 is in 2^112 {-0x9f19e1022000,...,0xfa22f1ff7000}
# live registers: 5 int64, 18 double

# input line 736: 

# input line 737:   z1 = d1
# z1#3!%d30 = d1#3!spill120
ldd [%fp+1903],%d30
# live registers: 5 int64, 19 double

# input line 738:   exact h0 += sr3lowx2
# h0#5!%d12 = h0#4!%d12 + sr3lowx2!%d34
faddd %d12,%d34,%d12
# range: h0#5!%d12 is in 2^0 {-0xb263960148000,...,0x7a353ee908000}
# live registers: 5 int64, 18 double

# input line 739: 

# input line 740:   z0 = d0
# z0#3!%d28 = d0#3!spill112
ldd [%fp+1911],%d28
# live registers: 5 int64, 19 double

# input line 741:   exact h1 += sr3highx2
# h1#5!%d14 = h1#4!%d14 + sr3highx2!%d32
faddd %d14,%d32,%d14
# range: h1#5!%d14 is in 2^16 {-0xbfc44c315400,...,0x12fc8a9a9b400}
# live registers: 5 int64, 18 double

# input line 742:   

# input line 743:   z1 -= alpha32
# z1#4!%d30 = z1#3!%d30 - alpha32!%d4
fsubd %d30,%d4,%d30
# range: z1#4!%d30 is in 2^32 {0,...,0xffffffff}
# live registers: 5 int64, 18 double

# input line 744:   

# input line 745:   z0 -= alpha0
# z0#4!%d2 = z0#3!%d28 - alpha0#2!%d2
fsubd %d28,%d2,%d2
# range: z0#4!%d2 is in 2^0 {0,...,0xffffffff}
# live registers: 5 int64, 17 double

# input line 746: 

# input line 747:   range z0 0 0 0xffffffff
# live registers: 5 int64, 17 double

# input line 748:   range z1 32 0 0xffffffff
# live registers: 5 int64, 17 double

# input line 749:   range z2 64 0 0xffffffff
# live registers: 5 int64, 17 double

# input line 750:   range z3 96 0 0x1ffffffff
# live registers: 5 int64, 17 double

# input line 751: 

# input line 752: flags l - 16
# flags l!%i4 - 16
subcc %i4,16,%g0
# live registers: 5 int64, 17 double

# input line 753:   exact h5 += z3
# h5#2!%d22 = h5#5!%d22 + z3#4!%d62
faddd %d22,%d62,%d22
# range: h5#2!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x3167d19c6b800}
# live registers: 5 int64, 16 double

# input line 754: 

# input line 755:   exact h3 += z2
# h3#2!%d18 = h3#5!%d18 + z2#4!%d60
faddd %d18,%d60,%d18
# range: h3#2!%d18 is in 2^48 {-0xb8c50e67a400,...,0x223096874bc00}
# live registers: 5 int64, 15 double

# input line 756: 

# input line 757:   exact h1 += z1
# h1#2!%d14 = h1#5!%d14 + z1#4!%d30
faddd %d14,%d30,%d14
# range: h1#2!%d14 is in 2^16 {-0xbfc44c315400,...,0x22fc8a9a8b400}
# live registers: 5 int64, 14 double

# input line 758: 

# input line 759:   exact h0 += z0
# branch swapped from next instruction
bgeu,pt %xcc,._multiplyaddatleast16bytes
# range: h0#2!%d2 is in 2^0 {-0xb263960148000,...,0x7a354ee907fff}
# live registers: 5 int64, 13 double

# input line 760: 

# input line 761: goto multiplyaddatleast16bytes if uint64 >=
# h0#2!%d2 = h0#5!%d12 + z0#4!%d2
faddd %d12,%d2,%d2

# input line 762: 

# input line 763: multiplyaddatmost15bytes
._multiplyaddatmost15bytes:

# input line 764:   # block 4: multiplication after final full chunk

# input line 765: 

# input line 766:   y7 = h7 + alpha130
# y7#4!%d12 = h7!%d26 + alpha130!%d10
faddd %d26,%d10,%d12
# range: y7#4!%d12 is in 2^130 {0x17ffffd83987bf,...,0x1800003e88bc80}
# live registers: 5 int64, 14 double

# input line 767: 

# input line 768:   y6 = h6 + alpha130
# y6#4!%d28 = h6!%d24 + alpha130!%d10
faddd %d24,%d10,%d28
# range: y6#4!%d28 is in 2^130 {0x17fffffff90430,...,0x18000000044e2e}
# live registers: 5 int64, 15 double

# input line 769: 

# input line 770:   y1 = h1 + alpha32
# y1#3!%d30 = h1#2!%d14 + alpha32!%d4
faddd %d14,%d4,%d30
# range: y1#3!%d30 is in 2^32 {0x17ffff403bb3ce,...,0x1800022fc8a9a9}
# live registers: 5 int64, 16 double

# input line 771: 

# input line 772:   y0 = h0 + alpha32
# y0#3!%d32 = h0#2!%d2 + alpha32!%d4
faddd %d2,%d4,%d32
# range: y0#3!%d32 is in 2^32 {0x17fffffff4d9c6,...,0x1800000007a355}
# live registers: 5 int64, 17 double

# input line 773: 

# input line 774:   exact y7 -= alpha130
# y7#5!%d12 = y7#4!%d12 - alpha130!%d10
fsubd %d12,%d10,%d12
# range: y7#5!%d12 is in 2^130 {-0x27c67841,...,0x3e88bc80}
# live registers: 5 int64, 17 double

# input line 775: 

# input line 776:   exact y6 -= alpha130
# y6#5!%d28 = y6#4!%d28 - alpha130!%d10
fsubd %d28,%d10,%d28
# range: y6#5!%d28 is in 2^130 {-0x6fbd0,...,0x44e2e}
# live registers: 5 int64, 17 double

# input line 777: 

# input line 778:   exact y1 -= alpha32
# y1#4!%d30 = y1#3!%d30 - alpha32!%d4
fsubd %d30,%d4,%d30
# range: y1#4!%d30 is in 2^32 {-0xbfc44c32,...,0x22fc8a9a9}
# live registers: 5 int64, 17 double

# input line 779: 

# input line 780:   exact y0 -= alpha32
# y0#4!%d32 = y0#3!%d32 - alpha32!%d4
fsubd %d32,%d4,%d32
# range: y0#4!%d32 is in 2^32 {-0xb263a,...,0x7a355}
# live registers: 5 int64, 17 double

# input line 781: 

# input line 782:   y5 = h5 + alpha96
# y5#3!%d34 = h5#2!%d22 + alpha96!%d8
faddd %d22,%d8,%d34
# range: y5#3!%d34 is in 2^96 {0x17ffff4e1e4589,...,0x180003167d19c7}
# live registers: 5 int64, 18 double

# input line 783: 

# input line 784:   y4 = h4 + alpha96
# y4#3!%d36 = h4!%d20 + alpha96!%d8
faddd %d20,%d8,%d36
# range: y4#3!%d36 is in 2^96 {0x17ffffffeb5d70,...,0x180000000cd84b}
# live registers: 5 int64, 19 double

# input line 785: 

# input line 786:   exact x7 = h7 - y7
# x7#3!%d26 = h7!%d26 - y7#5!%d12
fsubd %d26,%d12,%d26
# range: x7#3!%d26 is in 2^112 {-0x20000,...,0x20000}
# live registers: 5 int64, 19 double

# input line 787:   exact y7 *= scale
# y7#6!%d12 = y7#5!%d12 * scale!%d0
fmuld %d12,%d0,%d12
# range: y7#6!%d12 is in 2^0 {-0xc6e05945,...,0x138abae80}
# live registers: 5 int64, 19 double

# input line 788: 

# input line 789:   exact x6 = h6 - y6
# x6#4!%d38 = h6!%d24 - y6#5!%d28
fsubd %d24,%d28,%d38
# range: x6#4!%d38 is in 2^96 {-0x200000000,...,0x200000000}
# live registers: 5 int64, 19 double

# input line 790:   exact y6 *= scale
# y6#6!%d24 = y6#5!%d28 * scale!%d0
fmuld %d28,%d0,%d24
# range: y6#6!%d24 is in 2^0 {-0x22eb10,...,0x1586e6}
# live registers: 5 int64, 19 double

# input line 791: 

# input line 792:   exact x1 = h1 - y1
# x1#3!%d28 = h1#2!%d14 - y1#4!%d30
fsubd %d14,%d30,%d28
# range: x1#3!%d28 is in 2^16 {-0x8000,...,0x8000}
# live registers: 5 int64, 19 double

# input line 793: 

# input line 794:   exact x0 = h0 - y0
# x0#4!%d40 = h0#2!%d2 - y0#4!%d32
fsubd %d2,%d32,%d40
# range: x0#4!%d40 is in 2^0 {-0x80000000,...,0x80000000}
# live registers: 5 int64, 19 double

# input line 795: 

# input line 796:   exact y5 -= alpha96
# y5#4!%d2 = y5#3!%d34 - alpha96!%d8
fsubd %d34,%d8,%d2
# range: y5#4!%d2 is in 2^96 {-0xb1e1ba77,...,0x3167d19c7}
# live registers: 5 int64, 19 double

# input line 797: 

# input line 798:   exact y4 -= alpha96
# y4#4!%d14 = y4#3!%d36 - alpha96!%d8
fsubd %d36,%d8,%d14
# range: y4#4!%d14 is in 2^96 {-0x14a290,...,0xcd84b}
# live registers: 5 int64, 19 double

# input line 799: 

# input line 800:   exact x1 += y7
# x1#4!%d28 = x1#3!%d28 + y7#6!%d12
faddd %d28,%d12,%d28
# range: x1#4!%d28 is in 2^0 {-0x146e05945,...,0x1b8abae80}
# live registers: 5 int64, 18 double

# input line 801: 

# input line 802:   exact x0 += y6
# x0#5!%d34 = x0#4!%d40 + y6#6!%d24
faddd %d40,%d24,%d34
# range: x0#5!%d34 is in 2^0 {-0x8022eb10,...,0x801586e6}
# live registers: 5 int64, 17 double

# input line 803: 

# input line 804:   exact x7 += y5
# x7#4!%d12 = x7#3!%d26 + y5#4!%d2
faddd %d26,%d2,%d12
# range: x7#4!%d12 is in 2^96 {-0x2b1e1ba77,...,0x5167d19c7}
# live registers: 5 int64, 17 double

# input line 805: 

# input line 806:   exact x6 += y4
# x6#5!%d24 = x6#4!%d38 + y4#4!%d14
faddd %d38,%d14,%d24
# range: x6#5!%d24 is in 2^96 {-0x20014a290,...,0x2000cd84b}
# live registers: 5 int64, 17 double

# input line 807: 

# input line 808:   y3 = h3 + alpha64
# y3#3!%d36 = h3#2!%d18 + alpha64!%d6
faddd %d18,%d6,%d36
# range: y3#3!%d36 is in 2^64 {0x17ffff473af198,...,0x18000223096875}
# live registers: 5 int64, 18 double

# input line 809: 

# input line 810:   y2 = h2 + alpha64
# y2#3!%d38 = h2!%d16 + alpha64!%d6
faddd %d16,%d6,%d38
# range: y2#3!%d38 is in 2^64 {0x17fffffff0120e,...,0x180000000a430c}
# live registers: 5 int64, 19 double

# input line 811: 

# input line 812:   exact x0 += x1
# x0#6!%d28 = x0#5!%d34 + x1#4!%d28
faddd %d34,%d28,%d28
# range: x0#6!%d28 is in 2^0 {-0x1c7034455,...,0x238c13566}
# live registers: 5 int64, 18 double

# input line 813: 

# input line 814:   exact x6 += x7
# x6#6!%d26 = x6#5!%d24 + x7#4!%d12
faddd %d24,%d12,%d26
# range: x6#6!%d26 is in 2^96 {-0x4b1f65d07,...,0x71689f212}
# live registers: 5 int64, 17 double

# input line 815: 

# input line 816:   exact y3 -= alpha64
# y3#4!%d12 = y3#3!%d36 - alpha64!%d6
fsubd %d36,%d6,%d12
# range: y3#4!%d12 is in 2^64 {-0xb8c50e68,...,0x223096875}
# live registers: 5 int64, 17 double

# input line 817:   unspill r3low
# r3low#5!%d34 = r3low@spill!spill0
ldd [%fp+2023],%d34
# range: r3low#5!%d34 is in 2^98 {-0x2000,...,0x2000}
# live registers: 5 int64, 18 double

# input line 818: 

# input line 819:   exact y2 -= alpha64
# y2#4!%d24 = y2#3!%d38 - alpha64!%d6
fsubd %d38,%d6,%d24
# range: y2#4!%d24 is in 2^64 {-0xfedf2,...,0xa430c}
# live registers: 5 int64, 18 double

# input line 820:   unspill r0low
# r0low#5!%d36 = r0low@spill!spill8
ldd [%fp+2015],%d36
# range: r0low#5!%d36 is in 2^0 {-0x20000,...,0x20000}
# live registers: 5 int64, 19 double

# input line 821: 

# input line 822:   exact x5 = h5 - y5
# x5#3!%d22 = h5#2!%d22 - y5#4!%d2
fsubd %d22,%d2,%d22
# range: x5#3!%d22 is in 2^80 {-0x8000,...,0x8000}
# live registers: 5 int64, 18 double

# input line 823:   exact r3lowx0 = r3low * x0
# r3lowx0#2!%d38 = r3low#5!%d34 * x0#6!%d28
fmuld %d34,%d28,%d38
# range: r3lowx0#2!%d38 is in 2^98 {-0x471826acc000,...,0x38e0688aa000}
# live registers: 5 int64, 18 double

# input line 824:   unspill r3high
# r3high#4!%d2 = r3high@spill!spill16
ldd [%fp+2007],%d2
# range: r3high#4!%d2 is in 2^112 {0x0,...,0x1000}
# live registers: 5 int64, 19 double

# input line 825: 

# input line 826:   exact x4 = h4 - y4
# x4#4!%d20 = h4!%d20 - y4#4!%d14
fsubd %d20,%d14,%d20
# range: x4#4!%d20 is in 2^64 {-0x80000000,...,0x80000000}
# live registers: 5 int64, 18 double

# input line 827:   exact r0lowx6 = r0low * x6
# r0lowx6#2!%d40 = r0low#5!%d36 * x6#6!%d26
fmuld %d36,%d26,%d40
# range: r0lowx6#2!%d40 is in 2^96 {-0xe2d13e4240000,...,0x963ecba0e0000}
# live registers: 5 int64, 19 double

# input line 828:   unspill r0high
# r0high#4!%d34 = r0high@spill!spill24
ldd [%fp+1999],%d34
# range: r0high#4!%d34 is in 2^18 {0x0,...,0x400}
# live registers: 5 int64, 20 double

# input line 829: 

# input line 830:   exact x3 = h3 - y3
# x3#3!%d44 = h3#2!%d18 - y3#4!%d12
fsubd %d18,%d12,%d44
# range: x3#3!%d44 is in 2^48 {-0x8000,...,0x8000}
# live registers: 5 int64, 20 double

# input line 831:   exact r3highx0 = r3high * x0
# r3highx0#2!%d46 = r3high#4!%d2 * x0#6!%d28
fmuld %d2,%d28,%d46
# range: r3highx0#2!%d46 is in 2^112 {-0x1c7034455000,...,0x238c13566000}
# live registers: 5 int64, 20 double

# input line 832:   unspill sr1low
# sr1low#4!%d2 = sr1low@spill!spill32
ldd [%fp+1991],%d2
# range: sr1low#4!%d2 is in 2^-96 {-0x8000,...,0x8000}
# live registers: 5 int64, 21 double

# input line 833: 

# input line 834:   exact x2 = h2 - y2
# x2#4!%d48 = h2!%d16 - y2#4!%d24
fsubd %d16,%d24,%d48
# range: x2#4!%d48 is in 2^32 {-0x80000000,...,0x80000000}
# live registers: 5 int64, 21 double

# input line 835:   exact r0highx6 = r0high * x6
# r0highx6#2!%d50 = r0high#4!%d34 * x6#6!%d26
fmuld %d34,%d26,%d50
# range: r0highx6#2!%d50 is in 2^114 {-0x12c7d9741c00,...,0x1c5a27c84800}
# live registers: 5 int64, 22 double

# input line 836:   unspill sr1high
# sr1high#4!%d16 = sr1high@spill!spill40
ldd [%fp+1983],%d16
# range: sr1high#4!%d16 is in 2^-80 {0x0,...,0x1400}
# live registers: 5 int64, 23 double

# input line 837: 

# input line 838:   exact x5 += y3
# x5#4!%d14 = x5#3!%d22 + y3#4!%d12
faddd %d22,%d12,%d14
# range: x5#4!%d14 is in 2^64 {-0x138c50e68,...,0x2a3096875}
# live registers: 5 int64, 22 double

# input line 839:   exact r0lowx0 = r0low * x0
# r0lowx0#2!%d12 = r0low#5!%d36 * x0#6!%d28
fmuld %d36,%d28,%d12
# range: r0lowx0#2!%d12 is in 2^0 {-0x471826acc0000,...,0x38e0688aa0000}
# live registers: 5 int64, 23 double

# input line 840:   unspill r1low
# r1low#5!%d42 = r1low@spill!spill48
ldd [%fp+1975],%d42
# range: r1low#5!%d42 is in 2^34 {-0x8000,...,0x8000}
# live registers: 5 int64, 24 double

# input line 841: 

# input line 842:   exact h6 = r3lowx0 + r0lowx6
# h6#4!%d22 = r3lowx0#2!%d38 + r0lowx6#2!%d40
faddd %d38,%d40,%d22
# range: h6#4!%d22 is in 2^96 {-0xf49747ed70000,...,0xa476e5c388000}
# live registers: 5 int64, 23 double

# input line 843:   exact sr1lowx6 = sr1low * x6
# sr1lowx6#2!%d2 = sr1low#4!%d2 * x6#6!%d26
fmuld %d2,%d26,%d2
# range: sr1lowx6#2!%d2 is in 2^0 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 5 int64, 23 double

# input line 844:   unspill r1high
# r1high#4!%d38 = r1high@spill!spill56
ldd [%fp+1967],%d38
# range: r1high#4!%d38 is in 2^50 {0x0,...,0x400}
# live registers: 5 int64, 24 double

# input line 845: 

# input line 846:   exact x4 += y2
# x4#5!%d18 = x4#4!%d20 + y2#4!%d24
faddd %d20,%d24,%d18
# range: x4#5!%d18 is in 2^64 {-0x800fedf2,...,0x800a430c}
# live registers: 5 int64, 23 double

# input line 847:   exact r0highx0 = r0high * x0
# r0highx0#2!%d52 = r0high#4!%d34 * x0#6!%d28
fmuld %d34,%d28,%d52
# range: r0highx0#2!%d52 is in 2^18 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 5 int64, 24 double

# input line 848:   unspill sr2low
# sr2low#4!%d40 = sr2low@spill!spill64
ldd [%fp+1959],%d40
# range: sr2low#4!%d40 is in 2^-64 {-0x8000,...,0x8000}
# live registers: 5 int64, 25 double

# input line 849: 

# input line 850:   exact h7 = r3highx0 + r0highx6
# h7#4!%d24 = r3highx0#2!%d46 + r0highx6#2!%d50
faddd %d46,%d50,%d24
# range: h7#4!%d24 is in 2^112 {-0x678f9a15c000,...,0x94f4b2778000}
# live registers: 5 int64, 24 double

# input line 851:   exact sr1highx6 = sr1high * x6
# sr1highx6#2!%d54 = sr1high#4!%d16 * x6#6!%d26
fmuld %d16,%d26,%d54
# range: sr1highx6#2!%d54 is in 2^16 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 5 int64, 24 double

# input line 852:   unspill sr2high
# sr2high#4!%d46 = sr2high@spill!spill72
ldd [%fp+1951],%d46
# range: sr2high#4!%d46 is in 2^-48 {0x0,...,0x1400}
# live registers: 5 int64, 25 double

# input line 853: 

# input line 854:   exact x3 += y1
# x3#4!%d16 = x3#3!%d44 + y1#4!%d30
faddd %d44,%d30,%d16
# range: x3#4!%d16 is in 2^32 {-0x13fc44c32,...,0x2afc8a9a9}
# live registers: 5 int64, 24 double

# input line 855:   exact r1lowx0 = r1low * x0
# r1lowx0#2!%d56 = r1low#5!%d42 * x0#6!%d28
fmuld %d42,%d28,%d56
# range: r1lowx0#2!%d56 is in 2^34 {-0x11c609ab30000,...,0xe381a22a8000}
# live registers: 5 int64, 25 double

# input line 856:   unspill r2low
# r2low#5!%d44 = r2low@spill!spill80
ldd [%fp+1943],%d44
# range: r2low#5!%d44 is in 2^66 {-0x8000,...,0x8000}
# live registers: 5 int64, 26 double

# input line 857: 

# input line 858:   exact h0 = r0lowx0 + sr1lowx6
# h0#6!%d2 = r0lowx0#2!%d12 + sr1lowx6#2!%d2
faddd %d12,%d2,%d2
# range: h0#6!%d2 is in 2^0 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 5 int64, 25 double

# input line 859:   exact sr2lowx6 = sr2low * x6
# sr2lowx6#2!%d58 = sr2low#4!%d40 * x6#6!%d26
fmuld %d40,%d26,%d58
# range: sr2lowx6#2!%d58 is in 2^32 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 5 int64, 26 double

# input line 860:   unspill r2high
# r2high#4!%d50 = r2high@spill!spill88
ldd [%fp+1935],%d50
# range: r2high#4!%d50 is in 2^82 {0x0,...,0x400}
# live registers: 5 int64, 27 double

# input line 861: 

# input line 862:   exact x2 += y0
# x2#5!%d20 = x2#4!%d48 + y0#4!%d32
faddd %d48,%d32,%d20
# range: x2#5!%d20 is in 2^32 {-0x800b263a,...,0x8007a355}
# live registers: 5 int64, 26 double

# input line 863:   exact r1highx0 = r1high * x0
# r1highx0#2!%d60 = r1high#4!%d38 * x0#6!%d28
fmuld %d38,%d28,%d60
# range: r1highx0#2!%d60 is in 2^50 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 5 int64, 27 double

# input line 864:   unspill sr3low
# sr3low#4!%d48 = sr3low@spill!spill96
ldd [%fp+1927],%d48
# range: sr3low#4!%d48 is in 2^-32 {-0x8000,...,0x8000}
# live registers: 5 int64, 28 double

# input line 865: 

# input line 866:   exact h1 = r0highx0 + sr1highx6
# h1#6!%d12 = r0highx0#2!%d52 + sr1highx6#2!%d54
faddd %d52,%d54,%d12
# range: h1#6!%d12 is in 2^16 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 5 int64, 27 double

# input line 867:   exact sr2highx6 = sr2high * x6
# sr2highx6#2!%d54 = sr2high#4!%d46 * x6#6!%d26
fmuld %d46,%d26,%d54
# range: sr2highx6#2!%d54 is in 2^48 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 5 int64, 28 double

# input line 868:   unspill sr3high
# sr3high#4!%d52 = sr3high@spill!spill104
ldd [%fp+1919],%d52
# range: sr3high#4!%d52 is in 2^-16 {0x0,...,0x1400}
# live registers: 5 int64, 29 double

# input line 869: 

# input line 870:   exact x4 += x5
# x4#6!%d30 = x4#5!%d18 + x5#4!%d14
faddd %d18,%d14,%d30
# range: x4#6!%d30 is in 2^64 {-0x1b8d4fc5a,...,0x32313ab81}
# live registers: 5 int64, 28 double

# input line 871:   exact r2lowx0 = r2low * x0
# r2lowx0#2!%d18 = r2low#5!%d44 * x0#6!%d28
fmuld %d44,%d28,%d18
# range: r2lowx0#2!%d18 is in 2^66 {-0x11c609ab30000,...,0xe381a22a8000}
# live registers: 5 int64, 29 double

# input line 872: 

# input line 873:   exact h2 = r1lowx0 + sr2lowx6
# h2#4!%d14 = r1lowx0#2!%d56 + sr2lowx6#2!%d58
faddd %d56,%d58,%d14
# range: h2#4!%d14 is in 2^32 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 5 int64, 28 double

# input line 874:   exact sr3lowx6 = sr3low * x6
# sr3lowx6#2!%d56 = sr3low#4!%d48 * x6#6!%d26
fmuld %d48,%d26,%d56
# range: sr3lowx6#2!%d56 is in 2^64 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 5 int64, 29 double

# input line 875: 

# input line 876:   exact x2 += x3
# x2#6!%d32 = x2#5!%d20 + x3#4!%d16
faddd %d20,%d16,%d32
# range: x2#6!%d32 is in 2^32 {-0x1bfcf726c,...,0x32fd04cfe}
# live registers: 5 int64, 28 double

# input line 877:   exact r2highx0 = r2high * x0
# r2highx0#2!%d20 = r2high#4!%d50 * x0#6!%d28
fmuld %d50,%d28,%d20
# range: r2highx0#2!%d20 is in 2^82 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 5 int64, 28 double

# input line 878: 

# input line 879:   exact h3 = r1highx0 + sr2highx6
# h3#6!%d16 = r1highx0#2!%d60 + sr2highx6#2!%d54
faddd %d60,%d54,%d16
# range: h3#6!%d16 is in 2^48 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 5 int64, 27 double

# input line 880:   exact sr3highx6 = sr3high * x6
# sr3highx6#2!%d26 = sr3high#4!%d52 * x6#6!%d26
fmuld %d52,%d26,%d26
# range: sr3highx6#2!%d26 is in 2^80 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 5 int64, 27 double

# input line 881: 

# input line 882:   exact r1highx4 = r1high * x4
# r1highx4#2!%d28 = r1high#4!%d38 * x4#6!%d30
fmuld %d38,%d30,%d28
# range: r1highx4#2!%d28 is in 2^114 {-0x6e353f16800,...,0xc8c4eae0400}
# live registers: 5 int64, 28 double

# input line 883: 

# input line 884:   exact h4 = r2lowx0 + sr3lowx6
# h4#4!%d18 = r2lowx0#2!%d18 + sr3lowx6#2!%d56
faddd %d18,%d56,%d18
# range: h4#4!%d18 is in 2^64 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 5 int64, 27 double

# input line 885:   exact r1lowx4 = r1low * x4
# r1lowx4#2!%d54 = r1low#5!%d42 * x4#6!%d30
fmuld %d42,%d30,%d54
# range: r1lowx4#2!%d54 is in 2^98 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 5 int64, 28 double

# input line 886: 

# input line 887:   exact r0highx4 = r0high * x4
# r0highx4#2!%d56 = r0high#4!%d34 * x4#6!%d30
fmuld %d34,%d30,%d56
# range: r0highx4#2!%d56 is in 2^82 {-0x6e353f16800,...,0xc8c4eae0400}
# live registers: 5 int64, 29 double

# input line 888: 

# input line 889:   exact h5 = r2highx0 + sr3highx6
# h5#6!%d20 = r2highx0#2!%d20 + sr3highx6#2!%d26
faddd %d20,%d26,%d20
# range: h5#6!%d20 is in 2^80 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 5 int64, 28 double

# input line 890:   exact r0lowx4 = r0low * x4
# r0lowx4#2!%d58 = r0low#5!%d36 * x4#6!%d30
fmuld %d36,%d30,%d58
# range: r0lowx4#2!%d58 is in 2^64 {-0x6462757020000,...,0x371a9f8b40000}
# live registers: 5 int64, 29 double

# input line 891: 

# input line 892:   exact h7 += r1highx4
# h7#5!%d26 = h7#4!%d24 + r1highx4#2!%d28
faddd %d24,%d28,%d26
# range: h7#5!%d26 is in 2^112 {-0x831ce9db6000,...,0xc725ed2f9000}
# live registers: 5 int64, 28 double

# input line 893:   exact sr3highx4 = sr3high * x4
# sr3highx4#2!%d28 = sr3high#4!%d52 * x4#6!%d30
fmuld %d52,%d30,%d28
# range: sr3highx4#2!%d28 is in 2^48 {-0x2270a3b70800,...,0x3ebd89661400}
# live registers: 5 int64, 29 double

# input line 894: 

# input line 895:   exact h6 += r1lowx4
# h6#5!%d24 = h6#4!%d22 + r1lowx4#2!%d54
faddd %d22,%d54,%d24
# range: h6#5!%d24 is in 2^96 {-0x158f9bd5d90000,...,0xdb91854ec8000}
# live registers: 5 int64, 28 double

# input line 896:   exact sr3lowx4 = sr3low * x4
# sr3lowx4#2!%d54 = sr3low#4!%d48 * x4#6!%d30
fmuld %d48,%d30,%d54
# range: sr3lowx4#2!%d54 is in 2^32 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 5 int64, 29 double

# input line 897: 

# input line 898:   exact h5 += r0highx4
# h5#7!%d22 = h5#6!%d20 + r0highx4#2!%d56
faddd %d20,%d56,%d22
# range: h5#7!%d22 is in 2^80 {-0x95e4c34f7c00,...,0xe38014f7d800}
# live registers: 5 int64, 28 double

# input line 899:   exact sr2highx4 = sr2high * x4
# sr2highx4#2!%d46 = sr2high#4!%d46 * x4#6!%d30
fmuld %d46,%d30,%d46
# range: sr2highx4#2!%d46 is in 2^16 {-0x2270a3b70800,...,0x3ebd89661400}
# live registers: 5 int64, 28 double

# input line 900: 

# input line 901:   exact h4 += r0lowx4
# h4#5!%d20 = h4#4!%d18 + r0lowx4#2!%d58
faddd %d18,%d58,%d20
# range: h4#5!%d20 is in 2^64 {-0xe42eebad70000,...,0x958abafe18000}
# live registers: 5 int64, 27 double

# input line 902:   exact sr2lowx4 = sr2low * x4
# sr2lowx4#2!%d30 = sr2low#4!%d40 * x4#6!%d30
fmuld %d40,%d30,%d30
# range: sr2lowx4#2!%d30 is in 2^0 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 5 int64, 26 double

# input line 903: 

# input line 904:   exact h3 += sr3highx4
# h3#7!%d18 = h3#6!%d16 + sr3highx4#2!%d28
faddd %d16,%d28,%d18
# range: h3#7!%d18 is in 2^48 {-0x9cc81740e400,...,0xf00c63a5dc00}
# live registers: 5 int64, 25 double

# input line 905:   exact r0lowx2 = r0low * x2
# r0lowx2#2!%d28 = r0low#5!%d36 * x2#6!%d32
fmuld %d36,%d32,%d28
# range: r0lowx2#2!%d28 is in 2^32 {-0x65fa099fc0000,...,0x37f9ee4d80000}
# live registers: 5 int64, 25 double

# input line 906: 

# input line 907:   exact h2 += sr3lowx4
# h2#5!%d16 = h2#4!%d14 + sr3lowx4#2!%d54
faddd %d14,%d54,%d16
# range: h2#5!%d16 is in 2^32 {-0x98e5139958000,...,0x6c36c355a8000}
# live registers: 5 int64, 24 double

# input line 908:   exact r0highx2 = r0high * x2
# r0highx2#2!%d34 = r0high#4!%d34 * x2#6!%d32
fmuld %d34,%d32,%d34
# range: r0highx2#2!%d34 is in 2^50 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 5 int64, 24 double

# input line 909: 

# input line 910:   exact h1 += sr2highx4
# h1#7!%d14 = h1#6!%d12 + sr2highx4#2!%d46
faddd %d12,%d46,%d14
# range: h1#7!%d14 is in 2^16 {-0x9cc81740e400,...,0xf00c63a5dc00}
# live registers: 5 int64, 23 double

# input line 911:   exact r1lowx2 = r1low * x2
# r1lowx2#2!%d12 = r1low#5!%d42 * x2#6!%d32
fmuld %d42,%d32,%d12
# range: r1lowx2#2!%d12 is in 2^66 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 5 int64, 23 double

# input line 912: 

# input line 913:   exact h0 += sr2lowx4
# h0#7!%d2 = h0#6!%d2 + sr2lowx4#2!%d30
faddd %d2,%d30,%d2
# range: h0#7!%d2 is in 2^0 {-0x98e5139958000,...,0x6c36c355a8000}
# live registers: 5 int64, 22 double

# input line 914:   exact r1highx2 = r1high * x2
# r1highx2#2!%d30 = r1high#4!%d38 * x2#6!%d32
fmuld %d38,%d32,%d30
# range: r1highx2#2!%d30 is in 2^82 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 5 int64, 22 double

# input line 915: 

# input line 916:   exact h2 += r0lowx2
# h2!%d16 = h2#5!%d16 + r0lowx2#2!%d28
faddd %d16,%d28,%d16
# range: h2!%d16 is in 2^32 {-0xfedf1d3918000,...,0xa430b1a328000}
# live registers: 5 int64, 21 double

# input line 917:   exact r2lowx2 = r2low * x2
# r2lowx2#2!%d28 = r2low#5!%d44 * x2#6!%d32
fmuld %d44,%d32,%d28
# range: r2lowx2#2!%d28 is in 2^98 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 5 int64, 21 double

# input line 918: 

# input line 919:   exact h3 += r0highx2
# h3!%d18 = h3#7!%d18 + r0highx2#2!%d34
faddd %d18,%d34,%d18
# range: h3!%d18 is in 2^48 {-0xb8c50e67a400,...,0x123096875bc00}
# live registers: 5 int64, 20 double

# input line 920:   exact r2highx2 = r2high * x2
# r2highx2#2!%d34 = r2high#4!%d50 * x2#6!%d32
fmuld %d50,%d32,%d34
# range: r2highx2#2!%d34 is in 2^114 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 5 int64, 20 double

# input line 921: 

# input line 922:   exact h4 += r1lowx2
# h4!%d20 = h4#5!%d20 + r1lowx2#2!%d12
faddd %d20,%d12,%d20
# range: h4!%d20 is in 2^64 {-0x14a28f54d30000,...,0xcd84a94b98000}
# live registers: 5 int64, 19 double

# input line 923:   exact sr3lowx2 = sr3low * x2
# sr3lowx2#2!%d12 = sr3low#4!%d48 * x2#6!%d32
fmuld %d48,%d32,%d12
# range: sr3lowx2#2!%d12 is in 2^0 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 5 int64, 19 double

# input line 924: 

# input line 925:   exact h5 += r1highx2
# h5!%d22 = h5#7!%d22 + r1highx2#2!%d30
faddd %d22,%d30,%d22
# range: h5!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x1167d19c7b800}
# live registers: 5 int64, 18 double

# input line 926:   exact sr3highx2 = sr3high * x2
# sr3highx2#2!%d30 = sr3high#4!%d52 * x2#6!%d32
fmuld %d52,%d32,%d30
# range: sr3highx2#2!%d30 is in 2^16 {-0x22fc34f07000,...,0x3fbc4603d800}
# live registers: 5 int64, 17 double

# input line 927: 

# input line 928:   exact h6 += r2lowx2
# h6!%d24 = h6#5!%d24 + r2lowx2#2!%d28
faddd %d24,%d28,%d24
# range: h6!%d24 is in 2^96 {-0x1bef3c6fd50000,...,0x1138b739c48000}
# live registers: 5 int64, 16 double

# input line 929: 

# input line 930:   exact h7 += r2highx2
# h7!%d26 = h7#5!%d26 + r2highx2#2!%d34
faddd %d26,%d34,%d26
# range: h7!%d26 is in 2^112 {-0x9f19e1022000,...,0xfa22f1ff7000}
# live registers: 5 int64, 15 double

# input line 931: 

# input line 932:   exact h0 += sr3lowx2
# h0!%d12 = h0#7!%d2 + sr3lowx2#2!%d12
faddd %d2,%d12,%d12
# range: h0!%d12 is in 2^0 {-0xb263960148000,...,0x7a353ee908000}
# live registers: 5 int64, 14 double

# input line 933: 

# input line 934:   exact h1 += sr3highx2
# h1!%d14 = h1#7!%d14 + sr3highx2#2!%d30
faddd %d14,%d30,%d14
# range: h1!%d14 is in 2^16 {-0xbfc44c315400,...,0x12fc8a9a9b400}
# live registers: 5 int64, 13 double

# input line 935: 

# input line 936: addatmost15bytes
._addatmost15bytes:

# input line 937: 

# input line 938:   lbelow2 = l - 2
# lbelow2!%l0 = l!%i4 - 2
sub %i4,2,%l0
# live registers: 6 int64, 13 double

# input line 939: flags l - 0
# flags l!%i4 - 0
subcc %i4,0,%g0
# live registers: 6 int64, 13 double

# input line 940: 

# input line 941:   lbelow3 = l - 3
# branch swapped from next instruction
be,pt %xcc,._nomorebytes
# live registers: 7 int64, 13 double

# input line 942: goto nomorebytes if ==
# lbelow3!%l1 = l!%i4 - 3
sub %i4,3,%l1

# input line 943: 

# input line 944:   # block 5: partial-chunk handling

# input line 945: 

# input line 946:   (int64) lbelow2 >>= 63
# lbelow2#2!%l7 = (int64) lbelow2!%l0 >> 63
srax %l0,63,%l7
# live registers: 7 int64, 13 double

# input line 947:   lbelow4 = l - 4
# lbelow4!%l2 = l!%i4 - 4
sub %i4,4,%l2
# live registers: 8 int64, 13 double

# input line 948: 

# input line 949:   m00 = *(uchar *) (m + 0)
# m00#3!%l3 = *(uchar *) (m!%i3 + 0)
ldub [%i3+0],%l3
# live registers: 9 int64, 13 double

# input line 950:   (int64) lbelow3 >>= 63
# lbelow3#2!%o0 = (int64) lbelow3!%l1 >> 63
srax %l1,63,%o0
# live registers: 9 int64, 13 double

# input line 951:   m += lbelow2
# m#2!%l0 = m!%i3 + lbelow2#2!%l7
add %i3,%l7,%l0
# live registers: 9 int64, 13 double

# input line 952:   

# input line 953:   m01 = *(uchar *) (m + 1)
# m01#5!%l4 = *(uchar *) (m#2!%l0 + 1)
ldub [%l0+1],%l4
# live registers: 10 int64, 13 double

# input line 954:   (int64) lbelow4 >>= 63
# lbelow4#2!%o1 = (int64) lbelow4!%l2 >> 63
srax %l2,63,%o1
# live registers: 10 int64, 13 double

# input line 955:   m += lbelow3
# m#3!%l0 = m#2!%l0 + lbelow3#2!%o0
add %l0,%o0,%l0
# live registers: 10 int64, 13 double

# input line 956: 

# input line 957:   m02 = *(uchar *) (m + 2)
# m02#5!%l5 = *(uchar *) (m#3!%l0 + 2)
ldub [%l0+2],%l5
# live registers: 11 int64, 13 double

# input line 958:   m += lbelow4
# m#4!%l0 = m#3!%l0 + lbelow4#2!%o1
add %l0,%o1,%l0
# live registers: 11 int64, 13 double

# input line 959:   m0 = 2151
# m0#13!%l1 = 2151
add %g0,2151,%l1
# live registers: 12 int64, 13 double

# input line 960: 

# input line 961:   m03 = *(uchar *) (m + 3)
# m03#5!%l6 = *(uchar *) (m#4!%l0 + 3)
ldub [%l0+3],%l6
# live registers: 13 int64, 13 double

# input line 962:   m0 <<= 51
# m0#14!%l1 = m0#13!%l1 << 51
sllx %l1,51,%l1
# live registers: 13 int64, 13 double

# input line 963:   m1 = 2215
# m1#13!%l2 = 2215
add %g0,2215,%l2
# live registers: 14 int64, 13 double

# input line 964: 

# input line 965:   m0 += m00
# m0#15!%l1 = m0#14!%l1 + m00#3!%l3
add %l1,%l3,%l1
# live registers: 13 int64, 13 double

# input line 966:   m01 &= ~lbelow2
# m01#6!%l3 = m01#5!%l4 &~ lbelow2#2!%l7
andn %l4,%l7,%l3
# live registers: 13 int64, 13 double

# input line 967: 

# input line 968:   m02 &= ~lbelow3
# m02#6!%l4 = m02#5!%l5 &~ lbelow3#2!%o0
andn %l5,%o0,%l4
# live registers: 13 int64, 13 double

# input line 969:   m01 -= lbelow2
# m01#7!%l3 = m01#6!%l3 - lbelow2#2!%l7
sub %l3,%l7,%l3
# live registers: 13 int64, 13 double

# input line 970: 

# input line 971:   m01 <<= 8
# m01#8!%l3 = m01#7!%l3 << 8
sllx %l3,8,%l3
# live registers: 13 int64, 13 double

# input line 972:   m03 &= ~lbelow4
# m03#6!%l5 = m03#5!%l6 &~ lbelow4#2!%o1
andn %l6,%o1,%l5
# live registers: 13 int64, 13 double

# input line 973: 

# input line 974:   m0 += m01
# m0#16!%l1 = m0#15!%l1 + m01#8!%l3
add %l1,%l3,%l1
# live registers: 12 int64, 13 double

# input line 975:   lbelow2 -= lbelow3
# lbelow2#3!%l3 = lbelow2#2!%l7 - lbelow3#2!%o0
sub %l7,%o0,%l3
# live registers: 12 int64, 13 double

# input line 976: 

# input line 977:   m02 += lbelow2
# m02#7!%l3 = m02#6!%l4 + lbelow2#3!%l3
add %l4,%l3,%l3
# live registers: 11 int64, 13 double

# input line 978:   lbelow3 -= lbelow4
# lbelow3#3!%l4 = lbelow3#2!%o0 - lbelow4#2!%o1
sub %o0,%o1,%l4
# live registers: 11 int64, 13 double

# input line 979: 

# input line 980:   m02 <<= 16
# m02#8!%l3 = m02#7!%l3 << 16
sllx %l3,16,%l3
# live registers: 11 int64, 13 double

# input line 981:   m03 += lbelow3
# m03#7!%l4 = m03#6!%l5 + lbelow3#3!%l4
add %l5,%l4,%l4
# live registers: 10 int64, 13 double

# input line 982: 

# input line 983:   m03 <<= 24
# m03#8!%l4 = m03#7!%l4 << 24
sllx %l4,24,%l4
# live registers: 10 int64, 13 double

# input line 984:   m0 += m02
# m0#17!%l1 = m0#16!%l1 + m02#8!%l3
add %l1,%l3,%l1
# live registers: 9 int64, 13 double

# input line 985: 

# input line 986:   m0 += m03
# m0#18!%l1 = m0#17!%l1 + m03#8!%l4
add %l1,%l4,%l1
# live registers: 8 int64, 13 double

# input line 987:   lbelow5 = l - 5
# lbelow5!%l3 = l!%i4 - 5
sub %i4,5,%l3
# live registers: 9 int64, 13 double

# input line 988: 

# input line 989:   lbelow6 = l - 6
# lbelow6!%l4 = l!%i4 - 6
sub %i4,6,%l4
# live registers: 10 int64, 13 double

# input line 990:   lbelow7 = l - 7
# lbelow7!%l5 = l!%i4 - 7
sub %i4,7,%l5
# live registers: 11 int64, 13 double

# input line 991: 

# input line 992:   (int64) lbelow5 >>= 63
# lbelow5#2!%o0 = (int64) lbelow5!%l3 >> 63
srax %l3,63,%o0
# live registers: 11 int64, 13 double

# input line 993:   lbelow8 = l - 8
# lbelow8!%l6 = l!%i4 - 8
sub %i4,8,%l6
# live registers: 12 int64, 13 double

# input line 994: 

# input line 995:   (int64) lbelow6 >>= 63
# lbelow6#2!%o2 = (int64) lbelow6!%l4 >> 63
srax %l4,63,%o2
# live registers: 12 int64, 13 double

# input line 996:   m += lbelow5
# m#5!%l0 = m#4!%l0 + lbelow5#2!%o0
add %l0,%o0,%l0
# live registers: 12 int64, 13 double

# input line 997: 

# input line 998:   m10 = *(uchar *) (m + 4)
# m10#3!%l3 = *(uchar *) (m#5!%l0 + 4)
ldub [%l0+4],%l3
# live registers: 13 int64, 13 double

# input line 999:   (int64) lbelow7 >>= 63
# lbelow7#2!%o3 = (int64) lbelow7!%l5 >> 63
srax %l5,63,%o3
# live registers: 13 int64, 13 double

# input line 1000:   m += lbelow6
# m#6!%l0 = m#5!%l0 + lbelow6#2!%o2
add %l0,%o2,%l0
# live registers: 13 int64, 13 double

# input line 1001: 

# input line 1002:   m11 = *(uchar *) (m + 5)
# m11#5!%l4 = *(uchar *) (m#6!%l0 + 5)
ldub [%l0+5],%l4
# live registers: 14 int64, 13 double

# input line 1003:   (int64) lbelow8 >>= 63
# lbelow8#2!%o5 = (int64) lbelow8!%l6 >> 63
srax %l6,63,%o5
# live registers: 14 int64, 13 double

# input line 1004:   m += lbelow7
# m#7!%l0 = m#6!%l0 + lbelow7#2!%o3
add %l0,%o3,%l0
# live registers: 14 int64, 13 double

# input line 1005: 

# input line 1006:   m12 = *(uchar *) (m + 6)
# m12#5!%l5 = *(uchar *) (m#7!%l0 + 6)
ldub [%l0+6],%l5
# live registers: 15 int64, 13 double

# input line 1007:   m1 <<= 51
# m1#14!%l2 = m1#13!%l2 << 51
sllx %l2,51,%l2
# live registers: 15 int64, 13 double

# input line 1008:   m += lbelow8
# m#8!%l0 = m#7!%l0 + lbelow8#2!%o5
add %l0,%o5,%l0
# live registers: 15 int64, 13 double

# input line 1009: 

# input line 1010:   m13 = *(uchar *) (m + 7)
# m13#5!%l6 = *(uchar *) (m#8!%l0 + 7)
ldub [%l0+7],%l6
# live registers: 16 int64, 13 double

# input line 1011:   m10 &= ~lbelow5
# m10#4!%l3 = m10#3!%l3 &~ lbelow5#2!%o0
andn %l3,%o0,%l3
# live registers: 16 int64, 13 double

# input line 1012:   lbelow4 -= lbelow5
# lbelow4#3!%l7 = lbelow4#2!%o1 - lbelow5#2!%o0
sub %o1,%o0,%l7
# live registers: 16 int64, 13 double

# input line 1013: 

# input line 1014:   m10 += lbelow4
# m10#5!%l3 = m10#4!%l3 + lbelow4#3!%l7
add %l3,%l7,%l3
# live registers: 15 int64, 13 double

# input line 1015:   lbelow5 -= lbelow6
# lbelow5#3!%l7 = lbelow5#2!%o0 - lbelow6#2!%o2
sub %o0,%o2,%l7
# live registers: 15 int64, 13 double

# input line 1016: 

# input line 1017:   m11 &= ~lbelow6
# m11#6!%l4 = m11#5!%l4 &~ lbelow6#2!%o2
andn %l4,%o2,%l4
# live registers: 15 int64, 13 double

# input line 1018:   m11 += lbelow5
# m11#7!%l4 = m11#6!%l4 + lbelow5#3!%l7
add %l4,%l7,%l4
# live registers: 14 int64, 13 double

# input line 1019: 

# input line 1020:   m11 <<= 8
# m11#8!%l4 = m11#7!%l4 << 8
sllx %l4,8,%l4
# live registers: 14 int64, 13 double

# input line 1021:   m1 += m10
# m1#15!%l2 = m1#14!%l2 + m10#5!%l3
add %l2,%l3,%l2
# live registers: 13 int64, 13 double

# input line 1022: 

# input line 1023:   m1 += m11
# m1#16!%l2 = m1#15!%l2 + m11#8!%l4
add %l2,%l4,%l2
# live registers: 12 int64, 13 double

# input line 1024:   m12 &= ~lbelow7
# m12#6!%l3 = m12#5!%l5 &~ lbelow7#2!%o3
andn %l5,%o3,%l3
# live registers: 12 int64, 13 double

# input line 1025:   

# input line 1026:   lbelow6 -= lbelow7
# lbelow6#3!%l5 = lbelow6#2!%o2 - lbelow7#2!%o3
sub %o2,%o3,%l5
# live registers: 12 int64, 13 double

# input line 1027:   m13 &= ~lbelow8
# m13#6!%l4 = m13#5!%l6 &~ lbelow8#2!%o5
andn %l6,%o5,%l4
# live registers: 12 int64, 13 double

# input line 1028: 

# input line 1029:   m12 += lbelow6
# m12#7!%l3 = m12#6!%l3 + lbelow6#3!%l5
add %l3,%l5,%l3
# live registers: 11 int64, 13 double

# input line 1030:   lbelow7 -= lbelow8
# lbelow7#3!%l5 = lbelow7#2!%o3 - lbelow8#2!%o5
sub %o3,%o5,%l5
# live registers: 11 int64, 13 double

# input line 1031: 

# input line 1032:   m12 <<= 16
# m12#8!%l3 = m12#7!%l3 << 16
sllx %l3,16,%l3
# live registers: 11 int64, 13 double

# input line 1033:   m13 += lbelow7
# m13#7!%l4 = m13#6!%l4 + lbelow7#3!%l5
add %l4,%l5,%l4
# live registers: 10 int64, 13 double

# input line 1034: 

# input line 1035:   m13 <<= 24
# m13#8!%l4 = m13#7!%l4 << 24
sllx %l4,24,%l4
# live registers: 10 int64, 13 double

# input line 1036:   m1 += m12
# m1#17!%l2 = m1#16!%l2 + m12#8!%l3
add %l2,%l3,%l2
# live registers: 9 int64, 13 double

# input line 1037: 

# input line 1038:   m1 += m13
# m1#18!%l2 = m1#17!%l2 + m13#8!%l4
add %l2,%l4,%l2
# live registers: 8 int64, 13 double

# input line 1039:   m2 = 2279
# m2#13!%l3 = 2279
add %g0,2279,%l3
# live registers: 9 int64, 13 double

# input line 1040: 

# input line 1041:   lbelow9 = l - 9
# lbelow9!%l5 = l!%i4 - 9
sub %i4,9,%l5
# live registers: 10 int64, 13 double

# input line 1042:   m3 = 2343
# m3#13!%l4 = 2343
add %g0,2343,%l4
# live registers: 11 int64, 13 double

# input line 1043: 

# input line 1044:   lbelow10 = l - 10
# lbelow10!%l6 = l!%i4 - 10
sub %i4,10,%l6
# live registers: 12 int64, 13 double

# input line 1045:   lbelow11 = l - 11
# lbelow11!%l7 = l!%i4 - 11
sub %i4,11,%l7
# live registers: 13 int64, 13 double

# input line 1046: 

# input line 1047:   (int64) lbelow9 >>= 63
# lbelow9#2!%o2 = (int64) lbelow9!%l5 >> 63
srax %l5,63,%o2
# live registers: 13 int64, 13 double

# input line 1048:   lbelow12 = l - 12
# lbelow12!%o0 = l!%i4 - 12
sub %i4,12,%o0
# live registers: 14 int64, 13 double

# input line 1049: 

# input line 1050:   (int64) lbelow10 >>= 63
# lbelow10#2!%o3 = (int64) lbelow10!%l6 >> 63
srax %l6,63,%o3
# live registers: 14 int64, 13 double

# input line 1051:   m += lbelow9
# m#9!%l0 = m#8!%l0 + lbelow9#2!%o2
add %l0,%o2,%l0
# live registers: 14 int64, 13 double

# input line 1052: 

# input line 1053:   m20 = *(uchar *) (m + 8)
# m20#3!%l5 = *(uchar *) (m#9!%l0 + 8)
ldub [%l0+8],%l5
# live registers: 15 int64, 13 double

# input line 1054:   (int64) lbelow11 >>= 63
# lbelow11#2!%o7 = (int64) lbelow11!%l7 >> 63
srax %l7,63,%o7
# live registers: 15 int64, 13 double

# input line 1055:   m += lbelow10
# m#10!%l0 = m#9!%l0 + lbelow10#2!%o3
add %l0,%o3,%l0
# live registers: 15 int64, 13 double

# input line 1056: 

# input line 1057:   m21 = *(uchar *) (m + 9)
# m21#5!%l6 = *(uchar *) (m#10!%l0 + 9)
ldub [%l0+9],%l6
# live registers: 16 int64, 13 double

# input line 1058:   (int64) lbelow12 >>= 63
# lbelow12#2!%i1 = (int64) lbelow12!%o0 >> 63
srax %o0,63,%i1
# live registers: 16 int64, 13 double

# input line 1059:   m += lbelow11
# m#11!%l0 = m#10!%l0 + lbelow11#2!%o7
add %l0,%o7,%l0
# live registers: 16 int64, 13 double

# input line 1060: 

# input line 1061:   m22 = *(uchar *) (m + 10)
# m22#5!%l7 = *(uchar *) (m#11!%l0 + 10)
ldub [%l0+10],%l7
# live registers: 17 int64, 13 double

# input line 1062:   m2 <<= 51
# m2#14!%l3 = m2#13!%l3 << 51
sllx %l3,51,%l3
# live registers: 17 int64, 13 double

# input line 1063:   m += lbelow12
# m#12!%l0 = m#11!%l0 + lbelow12#2!%i1
add %l0,%i1,%l0
# live registers: 17 int64, 13 double

# input line 1064: 

# input line 1065:   m23 = *(uchar *) (m + 11)
# m23#5!%o0 = *(uchar *) (m#12!%l0 + 11)
ldub [%l0+11],%o0
# live registers: 18 int64, 13 double

# input line 1066:   m20 &= ~lbelow9
# m20#4!%l5 = m20#3!%l5 &~ lbelow9#2!%o2
andn %l5,%o2,%l5
# live registers: 18 int64, 13 double

# input line 1067:   lbelow8 -= lbelow9
# lbelow8#3!%o1 = lbelow8#2!%o5 - lbelow9#2!%o2
sub %o5,%o2,%o1
# live registers: 18 int64, 13 double

# input line 1068: 

# input line 1069:   m20 += lbelow8
# m20#5!%l5 = m20#4!%l5 + lbelow8#3!%o1
add %l5,%o1,%l5
# live registers: 17 int64, 13 double

# input line 1070:   lbelow9 -= lbelow10
# lbelow9#3!%o1 = lbelow9#2!%o2 - lbelow10#2!%o3
sub %o2,%o3,%o1
# live registers: 17 int64, 13 double

# input line 1071: 

# input line 1072:   m21 &= ~lbelow10
# m21#6!%l6 = m21#5!%l6 &~ lbelow10#2!%o3
andn %l6,%o3,%l6
# live registers: 17 int64, 13 double

# input line 1073:   m21 += lbelow9
# m21#7!%l6 = m21#6!%l6 + lbelow9#3!%o1
add %l6,%o1,%l6
# live registers: 16 int64, 13 double

# input line 1074: 

# input line 1075:   m21 <<= 8
# m21#8!%l6 = m21#7!%l6 << 8
sllx %l6,8,%l6
# live registers: 16 int64, 13 double

# input line 1076:   m2 += m20
# m2#15!%l3 = m2#14!%l3 + m20#5!%l5
add %l3,%l5,%l3
# live registers: 15 int64, 13 double

# input line 1077: 

# input line 1078:   m2 += m21
# m2#16!%l3 = m2#15!%l3 + m21#8!%l6
add %l3,%l6,%l3
# live registers: 14 int64, 13 double

# input line 1079:   m22 &= ~lbelow11
# m22#6!%l5 = m22#5!%l7 &~ lbelow11#2!%o7
andn %l7,%o7,%l5
# live registers: 14 int64, 13 double

# input line 1080:   

# input line 1081:   lbelow10 -= lbelow11
# lbelow10#3!%l7 = lbelow10#2!%o3 - lbelow11#2!%o7
sub %o3,%o7,%l7
# live registers: 14 int64, 13 double

# input line 1082:   m23 &= ~lbelow12
# m23#6!%l6 = m23#5!%o0 &~ lbelow12#2!%i1
andn %o0,%i1,%l6
# live registers: 14 int64, 13 double

# input line 1083: 

# input line 1084:   m22 += lbelow10
# m22#7!%l5 = m22#6!%l5 + lbelow10#3!%l7
add %l5,%l7,%l5
# live registers: 13 int64, 13 double

# input line 1085:   lbelow11 -= lbelow12
# lbelow11#3!%l7 = lbelow11#2!%o7 - lbelow12#2!%i1
sub %o7,%i1,%l7
# live registers: 13 int64, 13 double

# input line 1086: 

# input line 1087:   m22 <<= 16
# m22#8!%l5 = m22#7!%l5 << 16
sllx %l5,16,%l5
# live registers: 13 int64, 13 double

# input line 1088:   m23 += lbelow11
# m23#7!%l6 = m23#6!%l6 + lbelow11#3!%l7
add %l6,%l7,%l6
# live registers: 12 int64, 13 double

# input line 1089: 

# input line 1090:   m23 <<= 24
# m23#8!%l6 = m23#7!%l6 << 24
sllx %l6,24,%l6
# live registers: 12 int64, 13 double

# input line 1091:   m2 += m22
# m2#17!%l3 = m2#16!%l3 + m22#8!%l5
add %l3,%l5,%l3
# live registers: 11 int64, 13 double

# input line 1092: 

# input line 1093:   m3 <<= 51
# m3#14!%l4 = m3#13!%l4 << 51
sllx %l4,51,%l4
# live registers: 11 int64, 13 double

# input line 1094:   lbelow13 = l - 13
# lbelow13!%l5 = l!%i4 - 13
sub %i4,13,%l5
# live registers: 12 int64, 13 double

# input line 1095: 

# input line 1096:   (int64) lbelow13 >>= 63
# lbelow13#2!%o0 = (int64) lbelow13!%l5 >> 63
srax %l5,63,%o0
# live registers: 12 int64, 13 double

# input line 1097:   lbelow14 = l - 14
# lbelow14!%l5 = l!%i4 - 14
sub %i4,14,%l5
# live registers: 13 int64, 13 double

# input line 1098: 

# input line 1099:   (int64) lbelow14 >>= 63
# lbelow14#2!%o1 = (int64) lbelow14!%l5 >> 63
srax %l5,63,%o1
# live registers: 13 int64, 13 double

# input line 1100:   m += lbelow13
# m#13!%l0 = m#12!%l0 + lbelow13#2!%o0
add %l0,%o0,%l0
# live registers: 13 int64, 13 double

# input line 1101:   lbelow15 = l - 15
# lbelow15!%l7 = l!%i4 - 15
sub %i4,15,%l7
# live registers: 13 int64, 13 double

# input line 1102: 

# input line 1103:   m30 = *(uchar *) (m + 12)
# m30#3!%l5 = *(uchar *) (m#13!%l0 + 12)
ldub [%l0+12],%l5
# live registers: 14 int64, 13 double

# input line 1104:   (int64) lbelow15 >>= 63
# lbelow15#2!%o2 = (int64) lbelow15!%l7 >> 63
srax %l7,63,%o2
# live registers: 14 int64, 13 double

# input line 1105:   m += lbelow14
# m#14!%l0 = m#13!%l0 + lbelow14#2!%o1
add %l0,%o1,%l0
# live registers: 14 int64, 13 double

# input line 1106: 

# input line 1107:   m31 = *(uchar *) (m + 13)
# m31#5!%l7 = *(uchar *) (m#14!%l0 + 13)
ldub [%l0+13],%l7
# live registers: 15 int64, 13 double

# input line 1108:   m += lbelow15
# m#15!%l0 = m#14!%l0 + lbelow15#2!%o2
add %l0,%o2,%l0
# live registers: 15 int64, 13 double

# input line 1109:   m2 += m23
# m2#18!%l3 = m2#17!%l3 + m23#8!%l6
add %l3,%l6,%l3
# live registers: 14 int64, 13 double

# input line 1110: 

# input line 1111:   m32 = *(uchar *) (m + 14)
# m32#5!%l6 = *(uchar *) (m#15!%l0 + 14)
ldub [%l0+14],%l6
# live registers: 14 int64, 13 double

# input line 1112:   m30 &= ~lbelow13
# m30#4!%l0 = m30#3!%l5 &~ lbelow13#2!%o0
andn %l5,%o0,%l0
# live registers: 14 int64, 13 double

# input line 1113:   lbelow12 -= lbelow13
# lbelow12#3!%l5 = lbelow12#2!%i1 - lbelow13#2!%o0
sub %i1,%o0,%l5
# live registers: 14 int64, 13 double

# input line 1114: 

# input line 1115:   m30 += lbelow12
# m30#5!%l0 = m30#4!%l0 + lbelow12#3!%l5
add %l0,%l5,%l0
# live registers: 13 int64, 13 double

# input line 1116:   lbelow13 -= lbelow14
# lbelow13#3!%l5 = lbelow13#2!%o0 - lbelow14#2!%o1
sub %o0,%o1,%l5
# live registers: 13 int64, 13 double

# input line 1117: 

# input line 1118:   m3 += m30
# m3#15!%l0 = m3#14!%l4 + m30#5!%l0
add %l4,%l0,%l0
# live registers: 12 int64, 13 double

# input line 1119:   m31 &= ~lbelow14
# m31#6!%l4 = m31#5!%l7 &~ lbelow14#2!%o1
andn %l7,%o1,%l4
# live registers: 12 int64, 13 double

# input line 1120: 

# input line 1121:   m31 += lbelow13
# m31#7!%l4 = m31#6!%l4 + lbelow13#3!%l5
add %l4,%l5,%l4
# live registers: 11 int64, 13 double

# input line 1122:   m32 &= ~lbelow15
# m32#6!%l5 = m32#5!%l6 &~ lbelow15#2!%o2
andn %l6,%o2,%l5
# live registers: 11 int64, 13 double

# input line 1123: 

# input line 1124:   m31 <<= 8
# m31#8!%l4 = m31#7!%l4 << 8
sllx %l4,8,%l4
# live registers: 11 int64, 13 double

# input line 1125:   lbelow14 -= lbelow15
# lbelow14#3!%l6 = lbelow14#2!%o1 - lbelow15#2!%o2
sub %o1,%o2,%l6
# live registers: 11 int64, 13 double

# input line 1126: 

# input line 1127:   m3 += m31
# m3#16!%l0 = m3#15!%l0 + m31#8!%l4
add %l0,%l4,%l0
# live registers: 10 int64, 13 double

# input line 1128:   m32 += lbelow14
# m32#7!%l4 = m32#6!%l5 + lbelow14#3!%l6
add %l5,%l6,%l4
# live registers: 9 int64, 13 double

# input line 1129:   d0 = m0
# d0#4!spill112 = m0#18!%l1
stx %l1,[%fp+1911]
# live registers: 8 int64, 13 double

# input line 1130: 

# input line 1131:   m32 <<= 16
# m32#8!%l1 = m32#7!%l4 << 16
sllx %l4,16,%l1
# live registers: 8 int64, 13 double

# input line 1132:   m33 = lbelow15 + 1
# m33#7!%l4 = lbelow15#2!%o2 + 1
add %o2,1,%l4
# live registers: 8 int64, 13 double

# input line 1133:   d1 = m1
# d1#4!spill120 = m1#18!%l2
stx %l2,[%fp+1903]
# live registers: 7 int64, 13 double

# input line 1134: 

# input line 1135:   m33 <<= 24
# m33#8!%l2 = m33#7!%l4 << 24
sllx %l4,24,%l2
# live registers: 7 int64, 13 double

# input line 1136:   m3 += m32
# m3#17!%l0 = m3#16!%l0 + m32#8!%l1
add %l0,%l1,%l0
# live registers: 6 int64, 13 double

# input line 1137:   d2 = m2
# d2#4!spill128 = m2#18!%l3
stx %l3,[%fp+1895]
# live registers: 5 int64, 13 double

# input line 1138: 

# input line 1139:   m3 += m33
# m3#18!%l0 = m3#17!%l0 + m33#8!%l2
add %l0,%l2,%l0
# live registers: 4 int64, 13 double

# input line 1140:   d3 = m3
# d3#4!spill136 = m3#18!%l0
stx %l0,[%fp+1887]
# live registers: 3 int64, 13 double

# input line 1141: 

# input line 1142:   alpha0 = *(double *) (constants + 24)
# alpha0#3!%d2 = *(int64 *) (constants#4!%o4 + 24)
ldd [%o4+24],%d2
# range: alpha0#3!%d2 is in 2^51 {3,...,3}
# live registers: 3 int64, 14 double

# input line 1143:   range alpha0 51 3 3
# live registers: 3 int64, 14 double

# input line 1144: 

# input line 1145:   z3 = d3
# z3#5!%d34 = d3#4!spill136
ldd [%fp+1887],%d34
# live registers: 3 int64, 15 double

# input line 1146: 

# input line 1147:   z2 = d2
# z2#5!%d32 = d2#4!spill128
ldd [%fp+1895],%d32
# live registers: 3 int64, 16 double

# input line 1148: 

# input line 1149:   z1 = d1
# z1#5!%d30 = d1#4!spill120
ldd [%fp+1903],%d30
# live registers: 3 int64, 17 double

# input line 1150: 

# input line 1151:   z0 = d0
# z0#5!%d28 = d0#4!spill112
ldd [%fp+1911],%d28
# live registers: 3 int64, 18 double

# input line 1152: 

# input line 1153:   z3 -= alpha96
# z3#6!%d34 = z3#5!%d34 - alpha96!%d8
fsubd %d34,%d8,%d34
# range: z3#6!%d34 is in 2^96 {0,...,0x1ffffffff}
# live registers: 3 int64, 18 double

# input line 1154: 

# input line 1155:   z2 -= alpha64
# z2#6!%d32 = z2#5!%d32 - alpha64!%d6
fsubd %d32,%d6,%d32
# range: z2#6!%d32 is in 2^64 {0,...,0xffffffff}
# live registers: 3 int64, 18 double

# input line 1156: 

# input line 1157:   z1 -= alpha32
# z1#6!%d30 = z1#5!%d30 - alpha32!%d4
fsubd %d30,%d4,%d30
# range: z1#6!%d30 is in 2^32 {0,...,0xffffffff}
# live registers: 3 int64, 18 double

# input line 1158: 

# input line 1159:   z0 -= alpha0
# z0#6!%d2 = z0#5!%d28 - alpha0#3!%d2
fsubd %d28,%d2,%d2
# range: z0#6!%d2 is in 2^0 {0,...,0xffffffff}
# live registers: 3 int64, 17 double

# input line 1160:   

# input line 1161:   range z0 0 0 0xffffffff
# live registers: 3 int64, 17 double

# input line 1162:   range z1 32 0 0xffffffff
# live registers: 3 int64, 17 double

# input line 1163:   range z2 64 0 0xffffffff
# live registers: 3 int64, 17 double

# input line 1164:   range z3 96 0 0x1ffffffff
# live registers: 3 int64, 17 double

# input line 1165: 

# input line 1166:   exact h5 += z3
# h5#8!%d22 = h5!%d22 + z3#6!%d34
faddd %d22,%d34,%d22
# range: h5#8!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x3167d19c6b800}
# live registers: 3 int64, 16 double

# input line 1167: 

# input line 1168:   exact h3 += z2
# h3#8!%d18 = h3!%d18 + z2#6!%d32
faddd %d18,%d32,%d18
# range: h3#8!%d18 is in 2^48 {-0xb8c50e67a400,...,0x223096874bc00}
# live registers: 3 int64, 15 double

# input line 1169: 

# input line 1170:   exact h1 += z1
# h1#8!%d14 = h1!%d14 + z1#6!%d30
faddd %d14,%d30,%d14
# range: h1#8!%d14 is in 2^16 {-0xbfc44c315400,...,0x22fc8a9a8b400}
# live registers: 3 int64, 14 double

# input line 1171: 

# input line 1172:   exact h0 += z0
# h0#8!%d2 = h0!%d12 + z0#6!%d2
faddd %d12,%d2,%d2
# range: h0#8!%d2 is in 2^0 {-0xb263960148000,...,0x7a354ee907fff}
# live registers: 3 int64, 13 double

# input line 1173: 

# input line 1174:   y7 = h7 + alpha130
# y7#7!%d12 = h7!%d26 + alpha130!%d10
faddd %d26,%d10,%d12
# range: y7#7!%d12 is in 2^130 {0x17ffffd83987bf,...,0x1800003e88bc80}
# live registers: 3 int64, 14 double

# input line 1175: 

# input line 1176:   y6 = h6 + alpha130
# y6#7!%d28 = h6!%d24 + alpha130!%d10
faddd %d24,%d10,%d28
# range: y6#7!%d28 is in 2^130 {0x17fffffff90430,...,0x18000000044e2e}
# live registers: 3 int64, 15 double

# input line 1177: 

# input line 1178:   y1 = h1 + alpha32
# y1#5!%d30 = h1#8!%d14 + alpha32!%d4
faddd %d14,%d4,%d30
# range: y1#5!%d30 is in 2^32 {0x17ffff403bb3ce,...,0x1800022fc8a9a9}
# live registers: 3 int64, 16 double

# input line 1179: 

# input line 1180:   y0 = h0 + alpha32
# y0#5!%d32 = h0#8!%d2 + alpha32!%d4
faddd %d2,%d4,%d32
# range: y0#5!%d32 is in 2^32 {0x17fffffff4d9c6,...,0x1800000007a355}
# live registers: 3 int64, 17 double

# input line 1181: 

# input line 1182:   exact y7 -= alpha130
# y7#8!%d12 = y7#7!%d12 - alpha130!%d10
fsubd %d12,%d10,%d12
# range: y7#8!%d12 is in 2^130 {-0x27c67841,...,0x3e88bc80}
# live registers: 3 int64, 17 double

# input line 1183: 

# input line 1184:   exact y6 -= alpha130
# y6#8!%d28 = y6#7!%d28 - alpha130!%d10
fsubd %d28,%d10,%d28
# range: y6#8!%d28 is in 2^130 {-0x6fbd0,...,0x44e2e}
# live registers: 3 int64, 17 double

# input line 1185: 

# input line 1186:   exact y1 -= alpha32
# y1#6!%d30 = y1#5!%d30 - alpha32!%d4
fsubd %d30,%d4,%d30
# range: y1#6!%d30 is in 2^32 {-0xbfc44c32,...,0x22fc8a9a9}
# live registers: 3 int64, 17 double

# input line 1187: 

# input line 1188:   exact y0 -= alpha32
# y0#6!%d32 = y0#5!%d32 - alpha32!%d4
fsubd %d32,%d4,%d32
# range: y0#6!%d32 is in 2^32 {-0xb263a,...,0x7a355}
# live registers: 3 int64, 17 double

# input line 1189: 

# input line 1190:   y5 = h5 + alpha96
# y5#5!%d34 = h5#8!%d22 + alpha96!%d8
faddd %d22,%d8,%d34
# range: y5#5!%d34 is in 2^96 {0x17ffff4e1e4589,...,0x180003167d19c7}
# live registers: 3 int64, 18 double

# input line 1191: 

# input line 1192:   y4 = h4 + alpha96
# y4#5!%d36 = h4!%d20 + alpha96!%d8
faddd %d20,%d8,%d36
# range: y4#5!%d36 is in 2^96 {0x17ffffffeb5d70,...,0x180000000cd84b}
# live registers: 3 int64, 19 double

# input line 1193: 

# input line 1194:   exact x7 = h7 - y7
# x7#5!%d26 = h7!%d26 - y7#8!%d12
fsubd %d26,%d12,%d26
# range: x7#5!%d26 is in 2^112 {-0x20000,...,0x20000}
# live registers: 3 int64, 19 double

# input line 1195:   exact y7 *= scale
# y7#9!%d12 = y7#8!%d12 * scale!%d0
fmuld %d12,%d0,%d12
# range: y7#9!%d12 is in 2^0 {-0xc6e05945,...,0x138abae80}
# live registers: 3 int64, 19 double

# input line 1196: 

# input line 1197:   exact x6 = h6 - y6
# x6#7!%d38 = h6!%d24 - y6#8!%d28
fsubd %d24,%d28,%d38
# range: x6#7!%d38 is in 2^96 {-0x200000000,...,0x200000000}
# live registers: 3 int64, 19 double

# input line 1198:   exact y6 *= scale
# y6#9!%d24 = y6#8!%d28 * scale!%d0
fmuld %d28,%d0,%d24
# range: y6#9!%d24 is in 2^0 {-0x22eb10,...,0x1586e6}
# live registers: 3 int64, 19 double

# input line 1199: 

# input line 1200:   exact x1 = h1 - y1
# x1#5!%d28 = h1#8!%d14 - y1#6!%d30
fsubd %d14,%d30,%d28
# range: x1#5!%d28 is in 2^16 {-0x8000,...,0x8000}
# live registers: 3 int64, 19 double

# input line 1201: 

# input line 1202:   exact x0 = h0 - y0
# x0#7!%d40 = h0#8!%d2 - y0#6!%d32
fsubd %d2,%d32,%d40
# range: x0#7!%d40 is in 2^0 {-0x80000000,...,0x80000000}
# live registers: 3 int64, 19 double

# input line 1203: 

# input line 1204:   exact y5 -= alpha96
# y5#6!%d2 = y5#5!%d34 - alpha96!%d8
fsubd %d34,%d8,%d2
# range: y5#6!%d2 is in 2^96 {-0xb1e1ba77,...,0x3167d19c7}
# live registers: 3 int64, 19 double

# input line 1205: 

# input line 1206:   exact y4 -= alpha96
# y4#6!%d14 = y4#5!%d36 - alpha96!%d8
fsubd %d36,%d8,%d14
# range: y4#6!%d14 is in 2^96 {-0x14a290,...,0xcd84b}
# live registers: 3 int64, 19 double

# input line 1207: 

# input line 1208:   exact x1 += y7
# x1#6!%d28 = x1#5!%d28 + y7#9!%d12
faddd %d28,%d12,%d28
# range: x1#6!%d28 is in 2^0 {-0x146e05945,...,0x1b8abae80}
# live registers: 3 int64, 18 double

# input line 1209: 

# input line 1210:   exact x0 += y6
# x0#8!%d34 = x0#7!%d40 + y6#9!%d24
faddd %d40,%d24,%d34
# range: x0#8!%d34 is in 2^0 {-0x8022eb10,...,0x801586e6}
# live registers: 3 int64, 17 double

# input line 1211: 

# input line 1212:   exact x7 += y5
# x7#6!%d12 = x7#5!%d26 + y5#6!%d2
faddd %d26,%d2,%d12
# range: x7#6!%d12 is in 2^96 {-0x2b1e1ba77,...,0x5167d19c7}
# live registers: 3 int64, 17 double

# input line 1213: 

# input line 1214:   exact x6 += y4
# x6#8!%d24 = x6#7!%d38 + y4#6!%d14
faddd %d38,%d14,%d24
# range: x6#8!%d24 is in 2^96 {-0x20014a290,...,0x2000cd84b}
# live registers: 3 int64, 17 double

# input line 1215: 

# input line 1216:   y3 = h3 + alpha64
# y3#5!%d36 = h3#8!%d18 + alpha64!%d6
faddd %d18,%d6,%d36
# range: y3#5!%d36 is in 2^64 {0x17ffff473af198,...,0x18000223096875}
# live registers: 3 int64, 18 double

# input line 1217: 

# input line 1218:   y2 = h2 + alpha64
# y2#5!%d38 = h2!%d16 + alpha64!%d6
faddd %d16,%d6,%d38
# range: y2#5!%d38 is in 2^64 {0x17fffffff0120e,...,0x180000000a430c}
# live registers: 3 int64, 19 double

# input line 1219: 

# input line 1220:   exact x0 += x1
# x0#9!%d28 = x0#8!%d34 + x1#6!%d28
faddd %d34,%d28,%d28
# range: x0#9!%d28 is in 2^0 {-0x1c7034455,...,0x238c13566}
# live registers: 3 int64, 18 double

# input line 1221: 

# input line 1222:   exact x6 += x7
# x6#9!%d26 = x6#8!%d24 + x7#6!%d12
faddd %d24,%d12,%d26
# range: x6#9!%d26 is in 2^96 {-0x4b1f65d07,...,0x71689f212}
# live registers: 3 int64, 17 double

# input line 1223: 

# input line 1224:   exact y3 -= alpha64
# y3#6!%d12 = y3#5!%d36 - alpha64!%d6
fsubd %d36,%d6,%d12
# range: y3#6!%d12 is in 2^64 {-0xb8c50e68,...,0x223096875}
# live registers: 3 int64, 17 double

# input line 1225:   unspill r3low
# r3low#6!%d34 = r3low@spill!spill0
ldd [%fp+2023],%d34
# range: r3low#6!%d34 is in 2^98 {-0x2000,...,0x2000}
# live registers: 3 int64, 18 double

# input line 1226: 

# input line 1227:   exact y2 -= alpha64
# y2#6!%d24 = y2#5!%d38 - alpha64!%d6
fsubd %d38,%d6,%d24
# range: y2#6!%d24 is in 2^64 {-0xfedf2,...,0xa430c}
# live registers: 3 int64, 18 double

# input line 1228:   unspill r0low
# r0low#6!%d36 = r0low@spill!spill8
ldd [%fp+2015],%d36
# range: r0low#6!%d36 is in 2^0 {-0x20000,...,0x20000}
# live registers: 3 int64, 19 double

# input line 1229: 

# input line 1230:   exact x5 = h5 - y5
# x5#5!%d22 = h5#8!%d22 - y5#6!%d2
fsubd %d22,%d2,%d22
# range: x5#5!%d22 is in 2^80 {-0x8000,...,0x8000}
# live registers: 3 int64, 18 double

# input line 1231:   exact r3lowx0 = r3low * x0
# r3lowx0#3!%d38 = r3low#6!%d34 * x0#9!%d28
fmuld %d34,%d28,%d38
# range: r3lowx0#3!%d38 is in 2^98 {-0x471826acc000,...,0x38e0688aa000}
# live registers: 3 int64, 18 double

# input line 1232:   unspill r3high
# r3high#5!%d2 = r3high@spill!spill16
ldd [%fp+2007],%d2
# range: r3high#5!%d2 is in 2^112 {0x0,...,0x1000}
# live registers: 3 int64, 19 double

# input line 1233: 

# input line 1234:   exact x4 = h4 - y4
# x4#7!%d20 = h4!%d20 - y4#6!%d14
fsubd %d20,%d14,%d20
# range: x4#7!%d20 is in 2^64 {-0x80000000,...,0x80000000}
# live registers: 3 int64, 18 double

# input line 1235:   exact r0lowx6 = r0low * x6
# r0lowx6#3!%d40 = r0low#6!%d36 * x6#9!%d26
fmuld %d36,%d26,%d40
# range: r0lowx6#3!%d40 is in 2^96 {-0xe2d13e4240000,...,0x963ecba0e0000}
# live registers: 3 int64, 19 double

# input line 1236:   unspill r0high
# r0high#5!%d34 = r0high@spill!spill24
ldd [%fp+1999],%d34
# range: r0high#5!%d34 is in 2^18 {0x0,...,0x400}
# live registers: 3 int64, 20 double

# input line 1237: 

# input line 1238:   exact x3 = h3 - y3
# x3#5!%d44 = h3#8!%d18 - y3#6!%d12
fsubd %d18,%d12,%d44
# range: x3#5!%d44 is in 2^48 {-0x8000,...,0x8000}
# live registers: 3 int64, 20 double

# input line 1239:   exact r3highx0 = r3high * x0
# r3highx0#3!%d46 = r3high#5!%d2 * x0#9!%d28
fmuld %d2,%d28,%d46
# range: r3highx0#3!%d46 is in 2^112 {-0x1c7034455000,...,0x238c13566000}
# live registers: 3 int64, 20 double

# input line 1240:   unspill sr1low
# sr1low#5!%d2 = sr1low@spill!spill32
ldd [%fp+1991],%d2
# range: sr1low#5!%d2 is in 2^-96 {-0x8000,...,0x8000}
# live registers: 3 int64, 21 double

# input line 1241: 

# input line 1242:   exact x2 = h2 - y2
# x2#7!%d48 = h2!%d16 - y2#6!%d24
fsubd %d16,%d24,%d48
# range: x2#7!%d48 is in 2^32 {-0x80000000,...,0x80000000}
# live registers: 3 int64, 21 double

# input line 1243:   exact r0highx6 = r0high * x6
# r0highx6#3!%d50 = r0high#5!%d34 * x6#9!%d26
fmuld %d34,%d26,%d50
# range: r0highx6#3!%d50 is in 2^114 {-0x12c7d9741c00,...,0x1c5a27c84800}
# live registers: 3 int64, 22 double

# input line 1244:   unspill sr1high
# sr1high#5!%d16 = sr1high@spill!spill40
ldd [%fp+1983],%d16
# range: sr1high#5!%d16 is in 2^-80 {0x0,...,0x1400}
# live registers: 3 int64, 23 double

# input line 1245: 

# input line 1246:   exact x5 += y3
# x5#6!%d14 = x5#5!%d22 + y3#6!%d12
faddd %d22,%d12,%d14
# range: x5#6!%d14 is in 2^64 {-0x138c50e68,...,0x2a3096875}
# live registers: 3 int64, 22 double

# input line 1247:   exact r0lowx0 = r0low * x0
# r0lowx0#3!%d12 = r0low#6!%d36 * x0#9!%d28
fmuld %d36,%d28,%d12
# range: r0lowx0#3!%d12 is in 2^0 {-0x471826acc0000,...,0x38e0688aa0000}
# live registers: 3 int64, 23 double

# input line 1248:   unspill r1low
# r1low#6!%d42 = r1low@spill!spill48
ldd [%fp+1975],%d42
# range: r1low#6!%d42 is in 2^34 {-0x8000,...,0x8000}
# live registers: 3 int64, 24 double

# input line 1249: 

# input line 1250:   exact h6 = r3lowx0 + r0lowx6
# h6#6!%d22 = r3lowx0#3!%d38 + r0lowx6#3!%d40
faddd %d38,%d40,%d22
# range: h6#6!%d22 is in 2^96 {-0xf49747ed70000,...,0xa476e5c388000}
# live registers: 3 int64, 23 double

# input line 1251:   exact sr1lowx6 = sr1low * x6
# sr1lowx6#3!%d2 = sr1low#5!%d2 * x6#9!%d26
fmuld %d2,%d26,%d2
# range: sr1lowx6#3!%d2 is in 2^0 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 3 int64, 23 double

# input line 1252:   unspill r1high
# r1high#5!%d38 = r1high@spill!spill56
ldd [%fp+1967],%d38
# range: r1high#5!%d38 is in 2^50 {0x0,...,0x400}
# live registers: 3 int64, 24 double

# input line 1253: 

# input line 1254:   exact x4 += y2
# x4#8!%d18 = x4#7!%d20 + y2#6!%d24
faddd %d20,%d24,%d18
# range: x4#8!%d18 is in 2^64 {-0x800fedf2,...,0x800a430c}
# live registers: 3 int64, 23 double

# input line 1255:   exact r0highx0 = r0high * x0
# r0highx0#3!%d52 = r0high#5!%d34 * x0#9!%d28
fmuld %d34,%d28,%d52
# range: r0highx0#3!%d52 is in 2^18 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 3 int64, 24 double

# input line 1256:   unspill sr2low
# sr2low#5!%d40 = sr2low@spill!spill64
ldd [%fp+1959],%d40
# range: sr2low#5!%d40 is in 2^-64 {-0x8000,...,0x8000}
# live registers: 3 int64, 25 double

# input line 1257: 

# input line 1258:   exact h7 = r3highx0 + r0highx6
# h7#6!%d24 = r3highx0#3!%d46 + r0highx6#3!%d50
faddd %d46,%d50,%d24
# range: h7#6!%d24 is in 2^112 {-0x678f9a15c000,...,0x94f4b2778000}
# live registers: 3 int64, 24 double

# input line 1259:   exact sr1highx6 = sr1high * x6
# sr1highx6#3!%d54 = sr1high#5!%d16 * x6#9!%d26
fmuld %d16,%d26,%d54
# range: sr1highx6#3!%d54 is in 2^16 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 3 int64, 24 double

# input line 1260:   unspill sr2high
# sr2high#5!%d46 = sr2high@spill!spill72
ldd [%fp+1951],%d46
# range: sr2high#5!%d46 is in 2^-48 {0x0,...,0x1400}
# live registers: 3 int64, 25 double

# input line 1261: 

# input line 1262:   exact x3 += y1
# x3#6!%d16 = x3#5!%d44 + y1#6!%d30
faddd %d44,%d30,%d16
# range: x3#6!%d16 is in 2^32 {-0x13fc44c32,...,0x2afc8a9a9}
# live registers: 3 int64, 24 double

# input line 1263:   exact r1lowx0 = r1low * x0
# r1lowx0#3!%d56 = r1low#6!%d42 * x0#9!%d28
fmuld %d42,%d28,%d56
# range: r1lowx0#3!%d56 is in 2^34 {-0x11c609ab30000,...,0xe381a22a8000}
# live registers: 3 int64, 25 double

# input line 1264:   unspill r2low
# r2low#6!%d44 = r2low@spill!spill80
ldd [%fp+1943],%d44
# range: r2low#6!%d44 is in 2^66 {-0x8000,...,0x8000}
# live registers: 3 int64, 26 double

# input line 1265: 

# input line 1266:   exact h0 = r0lowx0 + sr1lowx6
# h0#9!%d2 = r0lowx0#3!%d12 + sr1lowx6#3!%d2
faddd %d12,%d2,%d2
# range: h0#9!%d2 is in 2^0 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 3 int64, 25 double

# input line 1267:   exact sr2lowx6 = sr2low * x6
# sr2lowx6#3!%d58 = sr2low#5!%d40 * x6#9!%d26
fmuld %d40,%d26,%d58
# range: sr2lowx6#3!%d58 is in 2^32 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 3 int64, 26 double

# input line 1268:   unspill r2high
# r2high#5!%d50 = r2high@spill!spill88
ldd [%fp+1935],%d50
# range: r2high#5!%d50 is in 2^82 {0x0,...,0x400}
# live registers: 3 int64, 27 double

# input line 1269: 

# input line 1270:   exact x2 += y0
# x2#8!%d20 = x2#7!%d48 + y0#6!%d32
faddd %d48,%d32,%d20
# range: x2#8!%d20 is in 2^32 {-0x800b263a,...,0x8007a355}
# live registers: 3 int64, 26 double

# input line 1271:   exact r1highx0 = r1high * x0
# r1highx0#3!%d60 = r1high#5!%d38 * x0#9!%d28
fmuld %d38,%d28,%d60
# range: r1highx0#3!%d60 is in 2^50 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 3 int64, 27 double

# input line 1272:   unspill sr3low
# sr3low#5!%d48 = sr3low@spill!spill96
ldd [%fp+1927],%d48
# range: sr3low#5!%d48 is in 2^-32 {-0x8000,...,0x8000}
# live registers: 3 int64, 28 double

# input line 1273: 

# input line 1274:   exact h1 = r0highx0 + sr1highx6
# h1#9!%d12 = r0highx0#3!%d52 + sr1highx6#3!%d54
faddd %d52,%d54,%d12
# range: h1#9!%d12 is in 2^16 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 3 int64, 27 double

# input line 1275:   exact sr2highx6 = sr2high * x6
# sr2highx6#3!%d54 = sr2high#5!%d46 * x6#9!%d26
fmuld %d46,%d26,%d54
# range: sr2highx6#3!%d54 is in 2^48 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 3 int64, 28 double

# input line 1276:   unspill sr3high
# sr3high#5!%d52 = sr3high@spill!spill104
ldd [%fp+1919],%d52
# range: sr3high#5!%d52 is in 2^-16 {0x0,...,0x1400}
# live registers: 3 int64, 29 double

# input line 1277: 

# input line 1278:   exact x4 += x5
# x4#9!%d30 = x4#8!%d18 + x5#6!%d14
faddd %d18,%d14,%d30
# range: x4#9!%d30 is in 2^64 {-0x1b8d4fc5a,...,0x32313ab81}
# live registers: 3 int64, 28 double

# input line 1279:   exact r2lowx0 = r2low * x0
# r2lowx0#3!%d18 = r2low#6!%d44 * x0#9!%d28
fmuld %d44,%d28,%d18
# range: r2lowx0#3!%d18 is in 2^66 {-0x11c609ab30000,...,0xe381a22a8000}
# live registers: 3 int64, 29 double

# input line 1280: 

# input line 1281:   exact h2 = r1lowx0 + sr2lowx6
# h2#6!%d14 = r1lowx0#3!%d56 + sr2lowx6#3!%d58
faddd %d56,%d58,%d14
# range: h2#6!%d14 is in 2^32 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 3 int64, 28 double

# input line 1282:   exact sr3lowx6 = sr3low * x6
# sr3lowx6#3!%d56 = sr3low#5!%d48 * x6#9!%d26
fmuld %d48,%d26,%d56
# range: sr3lowx6#3!%d56 is in 2^64 {-0x38b44f9090000,...,0x258fb2e838000}
# live registers: 3 int64, 29 double

# input line 1283: 

# input line 1284:   exact x2 += x3
# x2#9!%d32 = x2#8!%d20 + x3#6!%d16
faddd %d20,%d16,%d32
# range: x2#9!%d32 is in 2^32 {-0x1bfcf726c,...,0x32fd04cfe}
# live registers: 3 int64, 28 double

# input line 1285:   exact r2highx0 = r2high * x0
# r2highx0#3!%d20 = r2high#5!%d50 * x0#9!%d28
fmuld %d50,%d28,%d20
# range: r2highx0#3!%d20 is in 2^82 {-0x71c0d115400,...,0x8e304d59800}
# live registers: 3 int64, 28 double

# input line 1286: 

# input line 1287:   exact h3 = r1highx0 + sr2highx6
# h3#9!%d16 = r1highx0#3!%d60 + sr2highx6#3!%d54
faddd %d60,%d54,%d16
# range: h3#9!%d16 is in 2^48 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 3 int64, 27 double

# input line 1288:   exact sr3highx6 = sr3high * x6
# sr3highx6#3!%d26 = sr3high#5!%d52 * x6#9!%d26
fmuld %d52,%d26,%d26
# range: sr3highx6#3!%d26 is in 2^80 {-0x5de73f448c00,...,0x8dc2c6e96800}
# live registers: 3 int64, 27 double

# input line 1289: 

# input line 1290:   exact r1highx4 = r1high * x4
# r1highx4#3!%d28 = r1high#5!%d38 * x4#9!%d30
fmuld %d38,%d30,%d28
# range: r1highx4#3!%d28 is in 2^114 {-0x6e353f16800,...,0xc8c4eae0400}
# live registers: 3 int64, 28 double

# input line 1291: 

# input line 1292:   exact h4 = r2lowx0 + sr3lowx6
# h4#6!%d18 = r2lowx0#3!%d18 + sr3lowx6#3!%d56
faddd %d18,%d56,%d18
# range: h4#6!%d18 is in 2^64 {-0x7fcc763d50000,...,0x5e701b72d8000}
# live registers: 3 int64, 27 double

# input line 1293:   exact r1lowx4 = r1low * x4
# r1lowx4#3!%d54 = r1low#6!%d42 * x4#9!%d30
fmuld %d42,%d30,%d54
# range: r1lowx4#3!%d54 is in 2^98 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 3 int64, 28 double

# input line 1294: 

# input line 1295:   exact r0highx4 = r0high * x4
# r0highx4#3!%d56 = r0high#5!%d34 * x4#9!%d30
fmuld %d34,%d30,%d56
# range: r0highx4#3!%d56 is in 2^82 {-0x6e353f16800,...,0xc8c4eae0400}
# live registers: 3 int64, 29 double

# input line 1296: 

# input line 1297:   exact h5 = r2highx0 + sr3highx6
# h5#9!%d20 = r2highx0#3!%d20 + sr3highx6#3!%d26
faddd %d20,%d26,%d20
# range: h5#9!%d20 is in 2^80 {-0x7a577389dc00,...,0xb14eda3fc800}
# live registers: 3 int64, 28 double

# input line 1298:   exact r0lowx4 = r0low * x4
# r0lowx4#3!%d58 = r0low#6!%d36 * x4#9!%d30
fmuld %d36,%d30,%d58
# range: r0lowx4#3!%d58 is in 2^64 {-0x6462757020000,...,0x371a9f8b40000}
# live registers: 3 int64, 29 double

# input line 1299: 

# input line 1300:   exact h7 += r1highx4
# h7#7!%d26 = h7#6!%d24 + r1highx4#3!%d28
faddd %d24,%d28,%d26
# range: h7#7!%d26 is in 2^112 {-0x831ce9db6000,...,0xc725ed2f9000}
# live registers: 3 int64, 28 double

# input line 1301:   exact sr3highx4 = sr3high * x4
# sr3highx4#3!%d28 = sr3high#5!%d52 * x4#9!%d30
fmuld %d52,%d30,%d28
# range: sr3highx4#3!%d28 is in 2^48 {-0x2270a3b70800,...,0x3ebd89661400}
# live registers: 3 int64, 29 double

# input line 1302: 

# input line 1303:   exact h6 += r1lowx4
# h6#7!%d24 = h6#6!%d22 + r1lowx4#3!%d54
faddd %d22,%d54,%d24
# range: h6#7!%d24 is in 2^96 {-0x158f9bd5d90000,...,0xdb91854ec8000}
# live registers: 3 int64, 28 double

# input line 1304:   exact sr3lowx4 = sr3low * x4
# sr3lowx4#3!%d54 = sr3low#5!%d48 * x4#9!%d30
fmuld %d48,%d30,%d54
# range: sr3lowx4#3!%d54 is in 2^32 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 3 int64, 29 double

# input line 1305: 

# input line 1306:   exact h5 += r0highx4
# h5#10!%d22 = h5#9!%d20 + r0highx4#3!%d56
faddd %d20,%d56,%d22
# range: h5#10!%d22 is in 2^80 {-0x95e4c34f7c00,...,0xe38014f7d800}
# live registers: 3 int64, 28 double

# input line 1307:   exact sr2highx4 = sr2high * x4
# sr2highx4#3!%d46 = sr2high#5!%d46 * x4#9!%d30
fmuld %d46,%d30,%d46
# range: sr2highx4#3!%d46 is in 2^16 {-0x2270a3b70800,...,0x3ebd89661400}
# live registers: 3 int64, 28 double

# input line 1308: 

# input line 1309:   exact h4 += r0lowx4
# h4#7!%d20 = h4#6!%d18 + r0lowx4#3!%d58
faddd %d18,%d58,%d20
# range: h4#7!%d20 is in 2^64 {-0xe42eebad70000,...,0x958abafe18000}
# live registers: 3 int64, 27 double

# input line 1310:   exact sr2lowx4 = sr2low * x4
# sr2lowx4#3!%d30 = sr2low#5!%d40 * x4#9!%d30
fmuld %d40,%d30,%d30
# range: sr2lowx4#3!%d30 is in 2^0 {-0x19189d5c08000,...,0xdc6a7e2d0000}
# live registers: 3 int64, 26 double

# input line 1311: 

# input line 1312:   exact h3 += sr3highx4
# h3#10!%d18 = h3#9!%d16 + sr3highx4#3!%d28
faddd %d16,%d28,%d18
# range: h3#10!%d18 is in 2^48 {-0x9cc81740e400,...,0xf00c63a5dc00}
# live registers: 3 int64, 25 double

# input line 1313:   exact r0lowx2 = r0low * x2
# r0lowx2#3!%d28 = r0low#6!%d36 * x2#9!%d32
fmuld %d36,%d32,%d28
# range: r0lowx2#3!%d28 is in 2^32 {-0x65fa099fc0000,...,0x37f9ee4d80000}
# live registers: 3 int64, 25 double

# input line 1314: 

# input line 1315:   exact h2 += sr3lowx4
# h2#7!%d16 = h2#6!%d14 + sr3lowx4#3!%d54
faddd %d14,%d54,%d16
# range: h2#7!%d16 is in 2^32 {-0x98e5139958000,...,0x6c36c355a8000}
# live registers: 3 int64, 24 double

# input line 1316:   exact r0highx2 = r0high * x2
# r0highx2#3!%d34 = r0high#5!%d34 * x2#9!%d32
fmuld %d34,%d32,%d34
# range: r0highx2#3!%d34 is in 2^50 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 3 int64, 24 double

# input line 1317: 

# input line 1318:   exact h1 += sr2highx4
# h1#10!%d14 = h1#9!%d12 + sr2highx4#3!%d46
faddd %d12,%d46,%d14
# range: h1#10!%d14 is in 2^16 {-0x9cc81740e400,...,0xf00c63a5dc00}
# live registers: 3 int64, 23 double

# input line 1319:   exact r1lowx2 = r1low * x2
# r1lowx2#3!%d12 = r1low#6!%d42 * x2#9!%d32
fmuld %d42,%d32,%d12
# range: r1lowx2#3!%d12 is in 2^66 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 3 int64, 23 double

# input line 1320: 

# input line 1321:   exact h0 += sr2lowx4
# h0#10!%d2 = h0#9!%d2 + sr2lowx4#3!%d30
faddd %d2,%d30,%d2
# range: h0#10!%d2 is in 2^0 {-0x98e5139958000,...,0x6c36c355a8000}
# live registers: 3 int64, 22 double

# input line 1322:   exact r1highx2 = r1high * x2
# r1highx2#3!%d30 = r1high#5!%d38 * x2#9!%d32
fmuld %d38,%d32,%d30
# range: r1highx2#3!%d30 is in 2^82 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 3 int64, 22 double

# input line 1323: 

# input line 1324:   exact h2 += r0lowx2
# h2!%d16 = h2#7!%d16 + r0lowx2#3!%d28
faddd %d16,%d28,%d16
# range: h2!%d16 is in 2^32 {-0xfedf1d3918000,...,0xa430b1a328000}
# live registers: 3 int64, 21 double

# input line 1325:   exact r2lowx2 = r2low * x2
# r2lowx2#3!%d28 = r2low#6!%d44 * x2#9!%d32
fmuld %d44,%d32,%d28
# range: r2lowx2#3!%d28 is in 2^98 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 3 int64, 21 double

# input line 1326: 

# input line 1327:   exact h3 += r0highx2
# h3!%d18 = h3#10!%d18 + r0highx2#3!%d34
faddd %d18,%d34,%d18
# range: h3!%d18 is in 2^48 {-0xb8c50e67a400,...,0x123096875bc00}
# live registers: 3 int64, 20 double

# input line 1328:   exact r2highx2 = r2high * x2
# r2highx2#3!%d34 = r2high#5!%d50 * x2#9!%d32
fmuld %d50,%d32,%d34
# range: r2highx2#3!%d34 is in 2^114 {-0x6ff3dc9b000,...,0xcbf4133f800}
# live registers: 3 int64, 20 double

# input line 1329: 

# input line 1330:   exact h4 += r1lowx2
# h4!%d20 = h4#7!%d20 + r1lowx2#3!%d12
faddd %d20,%d12,%d20
# range: h4!%d20 is in 2^64 {-0x14a28f54d30000,...,0xcd84a94b98000}
# live registers: 3 int64, 19 double

# input line 1331:   exact sr3lowx2 = sr3low * x2
# sr3lowx2#3!%d12 = sr3low#5!%d48 * x2#9!%d32
fmuld %d48,%d32,%d12
# range: sr3lowx2#3!%d12 is in 2^0 {-0x197e8267f0000,...,0xdfe7b9360000}
# live registers: 3 int64, 19 double

# input line 1332: 

# input line 1333:   exact h5 += r1highx2
# h5!%d22 = h5#10!%d22 + r1highx2#3!%d30
faddd %d22,%d30,%d22
# range: h5!%d22 is in 2^80 {-0xb1e1ba763c00,...,0x1167d19c7b800}
# live registers: 3 int64, 18 double

# input line 1334:   exact sr3highx2 = sr3high * x2
# sr3highx2#3!%d30 = sr3high#5!%d52 * x2#9!%d32
fmuld %d52,%d32,%d30
# range: sr3highx2#3!%d30 is in 2^16 {-0x22fc34f07000,...,0x3fbc4603d800}
# live registers: 3 int64, 17 double

# input line 1335: 

# input line 1336:   exact h6 += r2lowx2
# h6!%d24 = h6#7!%d24 + r2lowx2#3!%d28
faddd %d24,%d28,%d24
# range: h6!%d24 is in 2^96 {-0x1bef3c6fd50000,...,0x1138b739c48000}
# live registers: 3 int64, 16 double

# input line 1337: 

# input line 1338:   exact h7 += r2highx2
# h7!%d26 = h7#7!%d26 + r2highx2#3!%d34
faddd %d26,%d34,%d26
# range: h7!%d26 is in 2^112 {-0x9f19e1022000,...,0xfa22f1ff7000}
# live registers: 3 int64, 15 double

# input line 1339: 

# input line 1340:   exact h0 += sr3lowx2
# h0!%d12 = h0#10!%d2 + sr3lowx2#3!%d12
faddd %d2,%d12,%d12
# range: h0!%d12 is in 2^0 {-0xb263960148000,...,0x7a353ee908000}
# live registers: 3 int64, 14 double

# input line 1341: 

# input line 1342:   exact h1 += sr3highx2
# h1!%d14 = h1#10!%d14 + sr3highx2#3!%d30
faddd %d14,%d30,%d14
# range: h1!%d14 is in 2^16 {-0xbfc44c315400,...,0x12fc8a9a9b400}
# live registers: 3 int64, 13 double

# input line 1343: 

# input line 1344: 

# input line 1345: nomorebytes
._nomorebytes:

# input line 1346:   # block 6: epilogue

# input line 1347: 

# input line 1348:   offset0 = *(double *) (constants + 104)
# offset0!%d42 = *(int64 *) (constants#4!%o4 + 104)
ldd [%o4+104],%d42
# range: offset0!%d42 is in 2^0 {0x180001fffffffb,...,0x180001fffffffb}
# live registers: 3 int64, 14 double

# input line 1349:   range offset0 0 0x180001fffffffb 0x180001fffffffb
# live registers: 3 int64, 14 double

# input line 1350:   y7 = h7 + alpha130
# y7#10!%d2 = h7!%d26 + alpha130!%d10
faddd %d26,%d10,%d2
# range: y7#10!%d2 is in 2^130 {0x17ffffd83987bf,...,0x1800003e88bc80}
# live registers: 3 int64, 15 double

# input line 1351:   

# input line 1352:   offset1 = *(double *) (constants + 112)
# offset1!%d44 = *(int64 *) (constants#4!%o4 + 112)
ldd [%o4+112],%d44
# range: offset1!%d44 is in 2^32 {0x180001fffffffe,...,0x180001fffffffe}
# live registers: 3 int64, 16 double

# input line 1353:   range offset1 32 0x180001fffffffe 0x180001fffffffe
# live registers: 3 int64, 16 double

# input line 1354:   y0 = h0 + alpha32
# y0#7!%d30 = h0!%d12 + alpha32!%d4
faddd %d12,%d4,%d30
# range: y0#7!%d30 is in 2^32 {0x17fffffff4d9c6,...,0x1800000007a354}
# live registers: 3 int64, 17 double

# input line 1355:   

# input line 1356:   offset2 = *(double *) (constants + 120)
# offset2!%d46 = *(int64 *) (constants#4!%o4 + 120)
ldd [%o4+120],%d46
# range: offset2!%d46 is in 2^64 {0x180001fffffffe,...,0x180001fffffffe}
# live registers: 3 int64, 18 double

# input line 1357:   range offset2 64 0x180001fffffffe 0x180001fffffffe
# live registers: 3 int64, 18 double

# input line 1358:   y1 = h1 + alpha32
# y1#7!%d28 = h1!%d14 + alpha32!%d4
faddd %d14,%d4,%d28
# range: y1#7!%d28 is in 2^32 {0x17ffff403bb3ce,...,0x1800012fc8a9aa}
# live registers: 3 int64, 19 double

# input line 1359:   

# input line 1360:   offset3 = *(double *) (constants + 128)
# offset3!%d48 = *(int64 *) (constants#4!%o4 + 128)
ldd [%o4+128],%d48
# range: offset3!%d48 is in 2^96 {0x180003fffffffe,...,0x180003fffffffe}
# live registers: 2 int64, 20 double

# input line 1361:   range offset3 96 0x180003fffffffe 0x180003fffffffe
# live registers: 2 int64, 20 double

# input line 1362:   y2 = h2 + alpha64
# y2#7!%d40 = h2!%d16 + alpha64!%d6
faddd %d16,%d6,%d40
# range: y2#7!%d40 is in 2^64 {0x17fffffff0120e,...,0x180000000a430c}
# live registers: 2 int64, 21 double

# input line 1363:   

# input line 1364:   exact y7 -= alpha130
# y7#11!%d2 = y7#10!%d2 - alpha130!%d10
fsubd %d2,%d10,%d2
# range: y7#11!%d2 is in 2^130 {-0x27c67841,...,0x3e88bc80}
# live registers: 2 int64, 21 double

# input line 1365:   

# input line 1366:   y3 = h3 + alpha64
# y3#7!%d38 = h3!%d18 + alpha64!%d6
faddd %d18,%d6,%d38
# range: y3#7!%d38 is in 2^64 {0x17ffff473af198,...,0x18000123096876}
# live registers: 2 int64, 22 double

# input line 1367:   

# input line 1368:   y4 = h4 + alpha96
# y4#7!%d34 = h4!%d20 + alpha96!%d8
faddd %d20,%d8,%d34
# range: y4#7!%d34 is in 2^96 {0x17ffffffeb5d70,...,0x180000000cd84b}
# live registers: 2 int64, 23 double

# input line 1369:   

# input line 1370:   y5 = h5 + alpha96
# y5#7!%d32 = h5!%d22 + alpha96!%d8
faddd %d22,%d8,%d32
# range: y5#7!%d32 is in 2^96 {0x17ffff4e1e4589,...,0x180001167d19c8}
# live registers: 2 int64, 24 double

# input line 1371:   

# input line 1372:   exact x7 = h7 - y7
# x7#7!%d36 = h7!%d26 - y7#11!%d2
fsubd %d26,%d2,%d36
# range: x7#7!%d36 is in 2^112 {-0x20000,...,0x20000}
# live registers: 2 int64, 24 double

# input line 1373:   exact y7 *= scale
# y7#12!%d26 = y7#11!%d2 * scale!%d0
fmuld %d2,%d0,%d26
# range: y7#12!%d26 is in 2^0 {-0xc6e05945,...,0x138abae80}
# live registers: 2 int64, 24 double

# input line 1374:   

# input line 1375:   exact y0 -= alpha32
# y0#8!%d30 = y0#7!%d30 - alpha32!%d4
fsubd %d30,%d4,%d30
# range: y0#8!%d30 is in 2^32 {-0xb263a,...,0x7a354}
# live registers: 2 int64, 24 double

# input line 1376:   

# input line 1377:   exact y1 -= alpha32
# y1#8!%d28 = y1#7!%d28 - alpha32!%d4
fsubd %d28,%d4,%d28
# range: y1#8!%d28 is in 2^32 {-0xbfc44c32,...,0x12fc8a9aa}
# live registers: 2 int64, 23 double

# input line 1378:   

# input line 1379:   exact y2 -= alpha64
# y2#8!%d40 = y2#7!%d40 - alpha64!%d6
fsubd %d40,%d6,%d40
# range: y2#8!%d40 is in 2^64 {-0xfedf2,...,0xa430c}
# live registers: 2 int64, 23 double

# input line 1380:   

# input line 1381:   exact h6 += x7
# h6#8!%d2 = h6!%d24 + x7#7!%d36
faddd %d24,%d36,%d2
# range: h6#8!%d2 is in 2^96 {-0x1bef3e6fd50000,...,0x1138b939c48000}
# live registers: 2 int64, 22 double

# input line 1382:   

# input line 1383:   exact y3 -= alpha64
# y3#8!%d36 = y3#7!%d38 - alpha64!%d6
fsubd %d38,%d6,%d36
# range: y3#8!%d36 is in 2^64 {-0xb8c50e68,...,0x123096876}
# live registers: 2 int64, 21 double

# input line 1384:   

# input line 1385:   exact y4 -= alpha96
# y4#8!%d24 = y4#7!%d34 - alpha96!%d8
fsubd %d34,%d8,%d24
# range: y4#8!%d24 is in 2^96 {-0x14a290,...,0xcd84b}
# live registers: 2 int64, 21 double

# input line 1386:   

# input line 1387:   exact y5 -= alpha96
# y5#8!%d6 = y5#7!%d32 - alpha96!%d8
fsubd %d32,%d8,%d6
# range: y5#8!%d6 is in 2^96 {-0xb1e1ba77,...,0x1167d19c8}
# live registers: 2 int64, 20 double

# input line 1388:   

# input line 1389:   y6 = h6 + alpha130
# y6#10!%d4 = h6#8!%d2 + alpha130!%d10
faddd %d2,%d10,%d4
# range: y6#10!%d4 is in 2^130 {0x17fffffff90430,...,0x18000000044e2f}
# live registers: 2 int64, 21 double

# input line 1390:   

# input line 1391:   exact x0 = h0 - y0
# x0#10!%d12 = h0!%d12 - y0#8!%d30
fsubd %d12,%d30,%d12
# range: x0#10!%d12 is in 2^0 {-0x80000000,...,0x80000000}
# live registers: 2 int64, 21 double

# input line 1392:   

# input line 1393:   exact x1 = h1 - y1
# x1#7!%d8 = h1!%d14 - y1#8!%d28
fsubd %d14,%d28,%d8
# range: x1#7!%d8 is in 2^16 {-0x8000,...,0x8000}
# live registers: 2 int64, 21 double

# input line 1394:   

# input line 1395:   exact x2 = h2 - y2
# x2#10!%d32 = h2!%d16 - y2#8!%d40
fsubd %d16,%d40,%d32
# range: x2#10!%d32 is in 2^32 {-0x80000000,...,0x80000000}
# live registers: 2 int64, 21 double

# input line 1396:   

# input line 1397:   exact y6 -= alpha130
# y6#11!%d4 = y6#10!%d4 - alpha130!%d10
fsubd %d4,%d10,%d4
# range: y6#11!%d4 is in 2^130 {-0x6fbd0,...,0x44e2f}
# live registers: 2 int64, 20 double

# input line 1398:   

# input line 1399:   exact x0 += y7
# x0#11!%d10 = x0#10!%d12 + y7#12!%d26
faddd %d12,%d26,%d10
# range: x0#11!%d10 is in 2^0 {-0x146e05945,...,0x1b8abae80}
# live registers: 2 int64, 19 double

# input line 1400:   

# input line 1401:   exact x3 = h3 - y3
# x3#7!%d16 = h3!%d18 - y3#8!%d36
fsubd %d18,%d36,%d16
# range: x3#7!%d16 is in 2^48 {-0x8000,...,0x8000}
# live registers: 2 int64, 19 double

# input line 1402:   

# input line 1403:   exact x4 = h4 - y4
# x4#10!%d14 = h4!%d20 - y4#8!%d24
fsubd %d20,%d24,%d14
# range: x4#10!%d14 is in 2^64 {-0x80000000,...,0x80000000}
# live registers: 2 int64, 19 double

# input line 1404:   

# input line 1405:   exact x5 = h5 - y5
# x5#7!%d12 = h5!%d22 - y5#8!%d6
fsubd %d22,%d6,%d12
# range: x5#7!%d12 is in 2^80 {-0x8000,...,0x8000}
# live registers: 2 int64, 19 double

# input line 1406:   

# input line 1407:   exact x6 = h6 - y6
# x6#10!%d2 = h6#8!%d2 - y6#11!%d4
fsubd %d2,%d4,%d2
# range: x6#10!%d2 is in 2^96 {-0x200000000,...,0x200000000}
# live registers: 2 int64, 19 double

# input line 1408:   

# input line 1409:   exact y6 *= scale
# y6#12!%d0 = y6#11!%d4 * scale!%d0
fmuld %d4,%d0,%d0
# range: y6#12!%d0 is in 2^0 {-0x22eb10,...,0x1586eb}
# live registers: 2 int64, 18 double

# input line 1410:   

# input line 1411:   exact x2 += y0
# x2#11!%d18 = x2#10!%d32 + y0#8!%d30
faddd %d32,%d30,%d18
# range: x2#11!%d18 is in 2^32 {-0x800b263a,...,0x8007a354}
# live registers: 2 int64, 17 double

# input line 1412:   

# input line 1413:   exact x3 += y1
# x3#8!%d16 = x3#7!%d16 + y1#8!%d28
faddd %d16,%d28,%d16
# range: x3#8!%d16 is in 2^32 {-0x13fc44c32,...,0x1afc8a9aa}
# live registers: 2 int64, 16 double

# input line 1414:   

# input line 1415:   exact x4 += y2
# x4#11!%d14 = x4#10!%d14 + y2#8!%d40
faddd %d14,%d40,%d14
# range: x4#11!%d14 is in 2^64 {-0x800fedf2,...,0x800a430c}
# live registers: 2 int64, 15 double

# input line 1416:   

# input line 1417:   exact x0 += y6
# x0#12!%d4 = x0#11!%d10 + y6#12!%d0
faddd %d10,%d0,%d4
# range: x0#12!%d4 is in 2^0 {-0x147034455,...,0x1b8c1356b}
# live registers: 2 int64, 14 double

# input line 1418:   

# input line 1419:   exact x5 += y3
# x5#8!%d10 = x5#7!%d12 + y3#8!%d36
faddd %d12,%d36,%d10
# range: x5#8!%d10 is in 2^64 {-0x138c50e68,...,0x1a3096876}
# live registers: 2 int64, 13 double

# input line 1420:   

# input line 1421:   exact x6 += y4
# x6#11!%d0 = x6#10!%d2 + y4#8!%d24
faddd %d2,%d24,%d0
# range: x6#11!%d0 is in 2^96 {-0x20014a290,...,0x2000cd84b}
# live registers: 2 int64, 12 double

# input line 1422:   

# input line 1423:   exact x2 += x3
# x2#12!%d12 = x2#11!%d18 + x3#8!%d16
faddd %d18,%d16,%d12
# range: x2#12!%d12 is in 2^32 {-0x1bfcf726c,...,0x22fd04cfe}
# live registers: 2 int64, 11 double

# input line 1424:   

# input line 1425:   exact x0 += x1
# x0#13!%d2 = x0#12!%d4 + x1#7!%d8
faddd %d4,%d8,%d2
# range: x0#13!%d2 is in 2^0 {-0x1c7034455,...,0x238c1356b}
# live registers: 2 int64, 10 double

# input line 1426:   

# input line 1427:   exact x4 += x5
# x4#12!%d4 = x4#11!%d14 + x5#8!%d10
faddd %d14,%d10,%d4
# range: x4#12!%d4 is in 2^64 {-0x1b8d4fc5a,...,0x22313ab82}
# live registers: 2 int64, 9 double

# input line 1428:   

# input line 1429:   exact x6 += y5
# x6#12!%d0 = x6#11!%d0 + y5#8!%d6
faddd %d0,%d6,%d0
# range: x6#12!%d0 is in 2^96 {-0x2b1f65d07,...,0x31689f213}
# live registers: 2 int64, 8 double

# input line 1430:   

# input line 1431:   exact x2 += offset1
# x2#13!%d6 = x2#12!%d12 + offset1!%d44
faddd %d12,%d44,%d6
# range: x2#13!%d6 is in 2^32 {0x18000040308d92,...,0x1800042fd04cfc}
# live registers: 2 int64, 7 double

# input line 1432:   d1 = x2
# d1#5!spill8 = x2#13!%d6
std %d6,[%fp+2015]
# range: d1#5!spill8 is in 2^32 {0x18000040308d92,...,0x1800042fd04cfc}
# live registers: 2 int64, 6 double

# input line 1433:   

# input line 1434:   exact x0 += offset0
# x0#14!%d2 = x0#13!%d2 + offset0!%d42
faddd %d2,%d42,%d2
# range: x0#14!%d2 is in 2^0 {0x18000038fcbba6,...,0x18000438c13566}
# live registers: 2 int64, 5 double

# input line 1435:   d0 = x0
# d0#5!spill0 = x0#14!%d2
std %d2,[%fp+2023]
# range: d0#5!spill0 is in 2^0 {0x18000038fcbba6,...,0x18000438c13566}
# live registers: 2 int64, 4 double

# input line 1436:   

# input line 1437:   exact x4 += offset2
# x4#13!%d2 = x4#12!%d4 + offset2!%d46
faddd %d4,%d46,%d2
# range: x4#13!%d2 is in 2^64 {0x180000472b03a4,...,0x1800042313ab80}
# live registers: 2 int64, 3 double

# input line 1438:   d2 = x4
# d2#5!spill16 = x4#13!%d2
std %d2,[%fp+2007]
# range: d2#5!spill16 is in 2^64 {0x180000472b03a4,...,0x1800042313ab80}
# live registers: 2 int64, 2 double

# input line 1439:   

# input line 1440:   exact x6 += offset3
# x6#13!%d0 = x6#12!%d0 + offset3!%d48
faddd %d0,%d48,%d0
# range: x6#13!%d0 is in 2^96 {0x1800014e09a2f7,...,0x1800071689f211}
# live registers: 2 int64, 1 double

# input line 1441:   d3 = x6
# d3#5!spill24 = x6#13!%d0
std %d0,[%fp+1999]
# range: d3#5!spill24 is in 2^96 {0x1800014e09a2f7,...,0x1800071689f211}
# live registers: 2 int64, 0 double

# input line 1442:   

# input line 1443:   

# input line 1444:   register int64 s00

# input line 1445:   register int64 s01

# input line 1446:   register int64 s02

# input line 1447:   register int64 s03

# input line 1448:   register int64 s10

# input line 1449:   register int64 s11

# input line 1450:   register int64 s12

# input line 1451:   register int64 s13

# input line 1452:   register int64 s20

# input line 1453:   register int64 s21

# input line 1454:   register int64 s22

# input line 1455:   register int64 s23

# input line 1456:   register int64 s30

# input line 1457:   register int64 s31

# input line 1458:   register int64 s32

# input line 1459:   register int64 s33

# input line 1460:   register int64 bits32

# input line 1461:   register int64 f

# input line 1462:   register int64 f0

# input line 1463:   register int64 f1

# input line 1464:   register int64 f2

# input line 1465:   register int64 f3

# input line 1466:   register int64 f4

# input line 1467:   register int64 g

# input line 1468:   register int64 g0

# input line 1469:   register int64 g1

# input line 1470:   register int64 g2

# input line 1471:   register int64 g3

# input line 1472:   register int64 g4

# input line 1473:   

# input line 1474:   

# input line 1475:   f0 = d0
# f0!%l2 = d0#5!spill0
ldx [%fp+2023],%l2
# live registers: 3 int64, 0 double

# input line 1476:   

# input line 1477:   f1 = d1
# f1!%l3 = d1#5!spill8
ldx [%fp+2015],%l3
# live registers: 4 int64, 0 double

# input line 1478:   bits32 = "-1"
# bits32!%l0 = -1
add %g0,-1,%l0
# live registers: 5 int64, 0 double

# input line 1479:   

# input line 1480:   f2 = d2
# f2!%l5 = d2#5!spill16
ldx [%fp+2007],%l5
# live registers: 6 int64, 0 double

# input line 1481:   bits32 = (uint64) bits32 >> 32
# bits32#2!%l0 = (uint64) bits32!%l0 >> 32
srlx %l0,32,%l0
# live registers: 6 int64, 0 double

# input line 1482:   

# input line 1483:   f3 = d3
# f3!%l6 = d3#5!spill24
ldx [%fp+1999],%l6
# live registers: 7 int64, 0 double

# input line 1484:   f = (uint64) f0 >> 32
# f!%l1 = (uint64) f0!%l2 >> 32
srlx %l2,32,%l1
# live registers: 8 int64, 0 double

# input line 1485:   

# input line 1486:   f0 &= bits32
# f0#2!%l2 = f0!%l2 & bits32#2!%l0
and %l2,%l0,%l2
# live registers: 8 int64, 0 double

# input line 1487:   f &= 255
# f#2!%l1 = f!%l1 & 255
and %l1,255,%l1
# live registers: 8 int64, 0 double

# input line 1488:   

# input line 1489:   f1 += f
# f1#2!%l3 = f1!%l3 + f#2!%l1
add %l3,%l1,%l3
# live registers: 7 int64, 0 double

# input line 1490:   g0 = f0 + 5
# g0!%l1 = f0#2!%l2 + 5
add %l2,5,%l1
# live registers: 8 int64, 0 double

# input line 1491:   

# input line 1492:   g = (uint64) g0 >> 32
# g!%l7 = (uint64) g0!%l1 >> 32
srlx %l1,32,%l7
# live registers: 9 int64, 0 double

# input line 1493:   g0 &= bits32
# g0#2!%o0 = g0!%l1 & bits32#2!%l0
and %l1,%l0,%o0
# live registers: 9 int64, 0 double

# input line 1494:   

# input line 1495:   f = (uint64) f1 >> 32
# f#3!%l1 = (uint64) f1#2!%l3 >> 32
srlx %l3,32,%l1
# live registers: 10 int64, 0 double

# input line 1496:   f1 &= bits32
# f1#3!%l4 = f1#2!%l3 & bits32#2!%l0
and %l3,%l0,%l4
# live registers: 10 int64, 0 double

# input line 1497:   

# input line 1498:   f &= 255
# f#4!%l1 = f#3!%l1 & 255
and %l1,255,%l1
# live registers: 10 int64, 0 double

# input line 1499:   g1 = f1 + g
# g1!%o1 = f1#3!%l4 + g!%l7
add %l4,%l7,%o1
# live registers: 10 int64, 0 double

# input line 1500:   

# input line 1501:   g = (uint64) g1 >> 32
# g#2!%l7 = (uint64) g1!%o1 >> 32
srlx %o1,32,%l7
# live registers: 11 int64, 0 double

# input line 1502:   f2 += f
# f2#2!%l3 = f2!%l5 + f#4!%l1
add %l5,%l1,%l3
# live registers: 10 int64, 0 double

# input line 1503:   

# input line 1504:   f = (uint64) f2 >> 32
# f#5!%l1 = (uint64) f2#2!%l3 >> 32
srlx %l3,32,%l1
# live registers: 11 int64, 0 double

# input line 1505:   g1 &= bits32
# g1#2!%o1 = g1!%o1 & bits32#2!%l0
and %o1,%l0,%o1
# live registers: 11 int64, 0 double

# input line 1506:   

# input line 1507:   f2 &= bits32
# f2#3!%l5 = f2#2!%l3 & bits32#2!%l0
and %l3,%l0,%l5
# live registers: 11 int64, 0 double

# input line 1508:   f &= 255
# f#6!%l1 = f#5!%l1 & 255
and %l1,255,%l1
# live registers: 11 int64, 0 double

# input line 1509:   

# input line 1510:   f3 += f
# f3#2!%l1 = f3!%l6 + f#6!%l1
add %l6,%l1,%l1
# live registers: 10 int64, 0 double

# input line 1511:   g2 = f2 + g
# g2!%l3 = f2#3!%l5 + g#2!%l7
add %l5,%l7,%l3
# live registers: 10 int64, 0 double

# input line 1512:   

# input line 1513:   g = (uint64) g2 >> 32
# g#3!%l6 = (uint64) g2!%l3 >> 32
srlx %l3,32,%l6
# live registers: 11 int64, 0 double

# input line 1514:   g2 &= bits32
# g2#2!%o4 = g2!%l3 & bits32#2!%l0
and %l3,%l0,%o4
# live registers: 11 int64, 0 double

# input line 1515:   

# input line 1516:   f4 = (uint64) f3 >> 32
# f4!%l3 = (uint64) f3#2!%l1 >> 32
srlx %l1,32,%l3
# live registers: 12 int64, 0 double

# input line 1517:   f3 &= bits32
# f3#3!%o2 = f3#2!%l1 & bits32#2!%l0
and %l1,%l0,%o2
# live registers: 12 int64, 0 double

# input line 1518:   

# input line 1519:   f4 &= 255
# f4#2!%l1 = f4!%l3 & 255
and %l3,255,%l1
# live registers: 12 int64, 0 double

# input line 1520:   g3 = f3 + g
# g3!%l6 = f3#3!%o2 + g#3!%l6
add %o2,%l6,%l6
# live registers: 12 int64, 0 double

# input line 1521:   

# input line 1522:   g = (uint64) g3 >> 32
# g#4!%l3 = (uint64) g3!%l6 >> 32
srlx %l6,32,%l3
# live registers: 13 int64, 0 double

# input line 1523:   g3 &= bits32
# g3#2!%o5 = g3!%l6 & bits32#2!%l0
and %l6,%l0,%o5
# live registers: 12 int64, 0 double

# input line 1524:   

# input line 1525:   g4 = f4 + g
# g4!%l0 = f4#2!%l1 + g#4!%l3
add %l1,%l3,%l0
# live registers: 11 int64, 0 double

# input line 1526:   

# input line 1527:   g4 = g4 - 4
# g4#2!%l1 = g4!%l0 - 4
sub %l0,4,%l1
# live registers: 11 int64, 0 double

# input line 1528:   s00 = *(uchar *) (s + 0)
# s00!%l0 = *(uchar *) (s!%i2 + 0)
ldub [%i2+0],%l0
# live registers: 12 int64, 0 double

# input line 1529:   

# input line 1530:   f = (int64) g4 >> 63
# f#7!%l6 = (int64) g4#2!%l1 >> 63
srax %l1,63,%l6
# live registers: 12 int64, 0 double

# input line 1531:   s01 = *(uchar *) (s + 1)
# s01!%l1 = *(uchar *) (s!%i2 + 1)
ldub [%i2+1],%l1
# live registers: 13 int64, 0 double

# input line 1532:   

# input line 1533:   f0 &= f
# f0#3!%l3 = f0#2!%l2 & f#7!%l6
and %l2,%l6,%l3
# live registers: 13 int64, 0 double

# input line 1534:   g0 &= ~f
# g0#3!%o0 = g0#2!%o0 &~ f#7!%l6
andn %o0,%l6,%o0
# live registers: 13 int64, 0 double

# input line 1535:   s02 = *(uchar *) (s + 2)
# s02!%l2 = *(uchar *) (s!%i2 + 2)
ldub [%i2+2],%l2
# live registers: 14 int64, 0 double

# input line 1536:   

# input line 1537:   f1 &= f
# f1#4!%l7 = f1#3!%l4 & f#7!%l6
and %l4,%l6,%l7
# live registers: 14 int64, 0 double

# input line 1538:   f0 |= g0
# f0#4!%o0 = f0#3!%l3 | g0#3!%o0
or %l3,%o0,%o0
# live registers: 13 int64, 0 double

# input line 1539:   s03 = *(uchar *) (s + 3)
# s03!%l3 = *(uchar *) (s!%i2 + 3)
ldub [%i2+3],%l3
# live registers: 14 int64, 0 double

# input line 1540:   

# input line 1541:   g1 &= ~f
# g1#3!%o3 = g1#2!%o1 &~ f#7!%l6
andn %o1,%l6,%o3
# live registers: 14 int64, 0 double

# input line 1542:   f2 &= f
# f2#4!%o1 = f2#3!%l5 & f#7!%l6
and %l5,%l6,%o1
# live registers: 14 int64, 0 double

# input line 1543:   s10 = *(uchar *) (s + 4)
# s10!%l4 = *(uchar *) (s!%i2 + 4)
ldub [%i2+4],%l4
# live registers: 15 int64, 0 double

# input line 1544:   

# input line 1545:   f3 &= f
# f3#4!%o2 = f3#3!%o2 & f#7!%l6
and %o2,%l6,%o2
# live registers: 15 int64, 0 double

# input line 1546:   g2 &= ~f
# g2#3!%o4 = g2#2!%o4 &~ f#7!%l6
andn %o4,%l6,%o4
# live registers: 15 int64, 0 double

# input line 1547:   s11 = *(uchar *) (s + 5)
# s11!%l5 = *(uchar *) (s!%i2 + 5)
ldub [%i2+5],%l5
# live registers: 16 int64, 0 double

# input line 1548:   

# input line 1549:   g3 &= ~f
# g3#3!%o5 = g3#2!%o5 &~ f#7!%l6
andn %o5,%l6,%o5
# live registers: 15 int64, 0 double

# input line 1550:   f1 |= g1
# f1#5!%o3 = f1#4!%l7 | g1#3!%o3
or %l7,%o3,%o3
# live registers: 14 int64, 0 double

# input line 1551:   s12 = *(uchar *) (s + 6)
# s12!%l6 = *(uchar *) (s!%i2 + 6)
ldub [%i2+6],%l6
# live registers: 15 int64, 0 double

# input line 1552:   

# input line 1553:   f2 |= g2
# f2#5!%o4 = f2#4!%o1 | g2#3!%o4
or %o1,%o4,%o4
# live registers: 14 int64, 0 double

# input line 1554:   f3 |= g3
# f3#5!%o5 = f3#4!%o2 | g3#3!%o5
or %o2,%o5,%o5
# live registers: 13 int64, 0 double

# input line 1555:   s13 = *(uchar *) (s + 7)
# s13!%l7 = *(uchar *) (s!%i2 + 7)
ldub [%i2+7],%l7
# live registers: 14 int64, 0 double

# input line 1556:   

# input line 1557:   s01 <<= 8
# s01#2!%l1 = s01!%l1 << 8
sllx %l1,8,%l1
# live registers: 14 int64, 0 double

# input line 1558:   f0 += s00
# f0#5!%o1 = f0#4!%o0 + s00!%l0
add %o0,%l0,%o1
# live registers: 13 int64, 0 double

# input line 1559:   s20 = *(uchar *) (s + 8)
# s20!%o0 = *(uchar *) (s!%i2 + 8)
ldub [%i2+8],%o0
# live registers: 14 int64, 0 double

# input line 1560:   

# input line 1561:   s02 <<= 16
# s02#2!%l0 = s02!%l2 << 16
sllx %l2,16,%l0
# live registers: 14 int64, 0 double

# input line 1562:   f0 += s01
# f0#6!%l2 = f0#5!%o1 + s01#2!%l1
add %o1,%l1,%l2
# live registers: 13 int64, 0 double

# input line 1563:   s21 = *(uchar *) (s + 9)
# s21!%o1 = *(uchar *) (s!%i2 + 9)
ldub [%i2+9],%o1
# live registers: 14 int64, 0 double

# input line 1564:   

# input line 1565:   s03 <<= 24
# s03#2!%l1 = s03!%l3 << 24
sllx %l3,24,%l1
# live registers: 14 int64, 0 double

# input line 1566:   f0 += s02
# f0#7!%o2 = f0#6!%l2 + s02#2!%l0
add %l2,%l0,%o2
# live registers: 13 int64, 0 double

# input line 1567:   s22 = *(uchar *) (s + 10)
# s22!%l3 = *(uchar *) (s!%i2 + 10)
ldub [%i2+10],%l3
# live registers: 14 int64, 0 double

# input line 1568:   

# input line 1569:   s11 <<= 8
# s11#2!%l0 = s11!%l5 << 8
sllx %l5,8,%l0
# live registers: 14 int64, 0 double

# input line 1570:   f1 += s10
# f1#6!%l5 = f1#5!%o3 + s10!%l4
add %o3,%l4,%l5
# live registers: 13 int64, 0 double

# input line 1571:   s23 = *(uchar *) (s + 11)
# s23!%l4 = *(uchar *) (s!%i2 + 11)
ldub [%i2+11],%l4
# live registers: 14 int64, 0 double

# input line 1572:   

# input line 1573:   s12 <<= 16
# s12#2!%l2 = s12!%l6 << 16
sllx %l6,16,%l2
# live registers: 14 int64, 0 double

# input line 1574:   f1 += s11
# f1#7!%l6 = f1#6!%l5 + s11#2!%l0
add %l5,%l0,%l6
# live registers: 13 int64, 0 double

# input line 1575:   s30 = *(uchar *) (s + 12)
# s30!%l5 = *(uchar *) (s!%i2 + 12)
ldub [%i2+12],%l5
# live registers: 14 int64, 0 double

# input line 1576:   

# input line 1577:   s13 <<= 24
# s13#2!%l0 = s13!%l7 << 24
sllx %l7,24,%l0
# live registers: 14 int64, 0 double

# input line 1578:   f1 += s12
# f1#8!%l6 = f1#7!%l6 + s12#2!%l2
add %l6,%l2,%l6
# live registers: 13 int64, 0 double

# input line 1579:   s31 = *(uchar *) (s + 13)
# s31!%l2 = *(uchar *) (s!%i2 + 13)
ldub [%i2+13],%l2
# live registers: 14 int64, 0 double

# input line 1580:   

# input line 1581:   f0 += s03
# f0#8!%o2 = f0#7!%o2 + s03#2!%l1
add %o2,%l1,%o2
# live registers: 13 int64, 0 double

# input line 1582:   f1 += s13
# f1#9!%o3 = f1#8!%l6 + s13#2!%l0
add %l6,%l0,%o3
# live registers: 12 int64, 0 double

# input line 1583:   s32 = *(uchar *) (s + 14)
# s32!%l6 = *(uchar *) (s!%i2 + 14)
ldub [%i2+14],%l6
# live registers: 13 int64, 0 double

# input line 1584:   

# input line 1585:   s21 <<= 8
# s21#2!%l0 = s21!%o1 << 8
sllx %o1,8,%l0
# live registers: 13 int64, 0 double

# input line 1586:   f2 += s20
# f2#6!%o0 = f2#5!%o4 + s20!%o0
add %o4,%o0,%o0
# live registers: 12 int64, 0 double

# input line 1587:   s33 = *(uchar *) (s + 15)
# s33!%l7 = *(uchar *) (s!%i2 + 15)
ldub [%i2+15],%l7
# live registers: 12 int64, 0 double

# input line 1588:   

# input line 1589:   s22 <<= 16
# s22#2!%l1 = s22!%l3 << 16
sllx %l3,16,%l1
# live registers: 12 int64, 0 double

# input line 1590:   f2 += s21
# f2#7!%l3 = f2#6!%o0 + s21#2!%l0
add %o0,%l0,%l3
# live registers: 11 int64, 0 double

# input line 1591:   

# input line 1592:   s23 <<= 24
# s23#2!%l0 = s23!%l4 << 24
sllx %l4,24,%l0
# live registers: 11 int64, 0 double

# input line 1593:   f2 += s22
# f2#8!%l3 = f2#7!%l3 + s22#2!%l1
add %l3,%l1,%l3
# live registers: 10 int64, 0 double

# input line 1594:   

# input line 1595:   s31 <<= 8
# s31#2!%l1 = s31!%l2 << 8
sllx %l2,8,%l1
# live registers: 10 int64, 0 double

# input line 1596:   f3 += s30
# f3#6!%l4 = f3#5!%o5 + s30!%l5
add %o5,%l5,%l4
# live registers: 9 int64, 0 double

# input line 1597:   

# input line 1598:   s32 <<= 16
# s32#2!%l2 = s32!%l6 << 16
sllx %l6,16,%l2
# live registers: 9 int64, 0 double

# input line 1599:   f3 += s31
# f3#7!%l4 = f3#6!%l4 + s31#2!%l1
add %l4,%l1,%l4
# live registers: 8 int64, 0 double

# input line 1600:   

# input line 1601:   s33 <<= 24
# s33#2!%l1 = s33!%l7 << 24
sllx %l7,24,%l1
# live registers: 8 int64, 0 double

# input line 1602:   f3 += s32
# f3#8!%l4 = f3#7!%l4 + s32#2!%l2
add %l4,%l2,%l4
# live registers: 7 int64, 0 double

# input line 1603:   

# input line 1604:   f2 += s23
# f2#9!%l2 = f2#8!%l3 + s23#2!%l0
add %l3,%l0,%l2
# live registers: 6 int64, 0 double

# input line 1605:   f3 += s33
# f3#9!%l1 = f3#8!%l4 + s33#2!%l1
add %l4,%l1,%l1
# live registers: 5 int64, 0 double

# input line 1606:   

# input line 1607:   *(uchar *) (out + 0) = f0
# *(uchar *) (out!%i0 + 0) = f0#8!%o2
stub %o2,[%i0+0]
# live registers: 5 int64, 0 double

# input line 1608:   (uint64) f0 >>= 8
# f0#9!%l0 = (uint64) f0#8!%o2 >> 8
srlx %o2,8,%l0
# live registers: 5 int64, 0 double

# input line 1609:   *(uchar *) (out + 1) = f0
# *(uchar *) (out!%i0 + 1) = f0#9!%l0
stub %l0,[%i0+1]
# live registers: 5 int64, 0 double

# input line 1610:   (uint64) f0 >>= 8
# f0#10!%l0 = (uint64) f0#9!%l0 >> 8
srlx %l0,8,%l0
# live registers: 5 int64, 0 double

# input line 1611:   *(uchar *) (out + 2) = f0
# *(uchar *) (out!%i0 + 2) = f0#10!%l0
stub %l0,[%i0+2]
# live registers: 5 int64, 0 double

# input line 1612:   (uint64) f0 >>= 8
# f0#11!%l0 = (uint64) f0#10!%l0 >> 8
srlx %l0,8,%l0
# live registers: 5 int64, 0 double

# input line 1613:   *(uchar *) (out + 3) = f0
# *(uchar *) (out!%i0 + 3) = f0#11!%l0
stub %l0,[%i0+3]
# live registers: 5 int64, 0 double

# input line 1614:   (uint64) f0 >>= 8
# f0#12!%l0 = (uint64) f0#11!%l0 >> 8
srlx %l0,8,%l0
# live registers: 5 int64, 0 double

# input line 1615:   f1 += f0
# f1#10!%l0 = f1#9!%o3 + f0#12!%l0
add %o3,%l0,%l0
# live registers: 4 int64, 0 double

# input line 1616:   

# input line 1617:   *(uchar *) (out + 4) = f1
# *(uchar *) (out!%i0 + 4) = f1#10!%l0
stub %l0,[%i0+4]
# live registers: 4 int64, 0 double

# input line 1618:   (uint64) f1 >>= 8
# f1#11!%l0 = (uint64) f1#10!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 1619:   *(uchar *) (out + 5) = f1
# *(uchar *) (out!%i0 + 5) = f1#11!%l0
stub %l0,[%i0+5]
# live registers: 4 int64, 0 double

# input line 1620:   (uint64) f1 >>= 8
# f1#12!%l0 = (uint64) f1#11!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 1621:   *(uchar *) (out + 6) = f1
# *(uchar *) (out!%i0 + 6) = f1#12!%l0
stub %l0,[%i0+6]
# live registers: 4 int64, 0 double

# input line 1622:   (uint64) f1 >>= 8
# f1#13!%l0 = (uint64) f1#12!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 1623:   *(uchar *) (out + 7) = f1
# *(uchar *) (out!%i0 + 7) = f1#13!%l0
stub %l0,[%i0+7]
# live registers: 4 int64, 0 double

# input line 1624:   (uint64) f1 >>= 8
# f1#14!%l0 = (uint64) f1#13!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 1625:   f2 += f1
# f2#10!%l0 = f2#9!%l2 + f1#14!%l0
add %l2,%l0,%l0
# live registers: 3 int64, 0 double

# input line 1626:   

# input line 1627:   *(uchar *) (out + 8) = f2
# *(uchar *) (out!%i0 + 8) = f2#10!%l0
stub %l0,[%i0+8]
# live registers: 3 int64, 0 double

# input line 1628:   (uint64) f2 >>= 8
# f2#11!%l0 = (uint64) f2#10!%l0 >> 8
srlx %l0,8,%l0
# live registers: 3 int64, 0 double

# input line 1629:   *(uchar *) (out + 9) = f2
# *(uchar *) (out!%i0 + 9) = f2#11!%l0
stub %l0,[%i0+9]
# live registers: 3 int64, 0 double

# input line 1630:   (uint64) f2 >>= 8
# f2#12!%l0 = (uint64) f2#11!%l0 >> 8
srlx %l0,8,%l0
# live registers: 3 int64, 0 double

# input line 1631:   *(uchar *) (out + 10) = f2
# *(uchar *) (out!%i0 + 10) = f2#12!%l0
stub %l0,[%i0+10]
# live registers: 3 int64, 0 double

# input line 1632:   (uint64) f2 >>= 8
# f2#13!%l0 = (uint64) f2#12!%l0 >> 8
srlx %l0,8,%l0
# live registers: 3 int64, 0 double

# input line 1633:   *(uchar *) (out + 11) = f2
# *(uchar *) (out!%i0 + 11) = f2#13!%l0
stub %l0,[%i0+11]
# live registers: 3 int64, 0 double

# input line 1634:   (uint64) f2 >>= 8
# f2#14!%l0 = (uint64) f2#13!%l0 >> 8
srlx %l0,8,%l0
# live registers: 3 int64, 0 double

# input line 1635:   f3 += f2
# f3#10!%l0 = f3#9!%l1 + f2#14!%l0
add %l1,%l0,%l0
# live registers: 2 int64, 0 double

# input line 1636:   

# input line 1637:   *(uchar *) (out + 12) = f3
# *(uchar *) (out!%i0 + 12) = f3#10!%l0
stub %l0,[%i0+12]
# live registers: 2 int64, 0 double

# input line 1638:   (uint64) f3 >>= 8
# f3#11!%l0 = (uint64) f3#10!%l0 >> 8
srlx %l0,8,%l0
# live registers: 2 int64, 0 double

# input line 1639:   *(uchar *) (out + 13) = f3
# *(uchar *) (out!%i0 + 13) = f3#11!%l0
stub %l0,[%i0+13]
# live registers: 2 int64, 0 double

# input line 1640:   (uint64) f3 >>= 8
# f3#12!%l0 = (uint64) f3#11!%l0 >> 8
srlx %l0,8,%l0
# live registers: 2 int64, 0 double

# input line 1641:   *(uchar *) (out + 14) = f3
# *(uchar *) (out!%i0 + 14) = f3#12!%l0
stub %l0,[%i0+14]
# live registers: 2 int64, 0 double

# input line 1642:   (uint64) f3 >>= 8
# f3#13!%l0 = (uint64) f3#12!%l0 >> 8
srlx %l0,8,%l0
# live registers: 2 int64, 0 double

# input line 1643:   *(uchar *) (out + 15) = f3
# *(uchar *) (out!%i0 + 15) = f3#13!%l0
stub %l0,[%i0+15]
# live registers: 0 int64, 0 double

# input line 1644:   

# input line 1645: 

# input line 1646: 

# input line 1647: leave
ret
restore
