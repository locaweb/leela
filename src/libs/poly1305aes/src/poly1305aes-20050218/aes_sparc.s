# aes_sparc.s version 20050131
# D. J. Bernstein
# Public domain.

# translated by qhasm-sparc version 20050131

# input line 1: register int64 out

# input line 2: register int64 k

# input line 3: register int64 n

# input line 4: register int64 x0

# input line 5: register int64 x1

# input line 6: register int64 x2

# input line 7: register int64 x3

# input line 8: register int64 y0

# input line 9: register int64 y1

# input line 10: register int64 y2

# input line 11: register int64 y3

# input line 12: register int64 loop4

# input line 13: register int64 table0

# input line 14: register int64 table1

# input line 15: register int64 table2

# input line 16: register int64 table3

# input line 17: register int64 byte1

# input line 18: register int64 byte2

# input line 19: register int64 byte3

# input line 20: register int64 e

# input line 21: register int64 p00

# input line 22: register int64 p01

# input line 23: register int64 p02

# input line 24: register int64 p03

# input line 25: register int64 z0

# input line 26: register int64 z1

# input line 27: register int64 z2

# input line 28: register int64 z3

# input line 29: register int64 p10

# input line 30: register int64 p11

# input line 31: register int64 p12

# input line 32: register int64 p13

# input line 33: register int64 p20

# input line 34: register int64 p21

# input line 35: register int64 p22

# input line 36: register int64 p23

# input line 37: register int64 p30

# input line 38: register int64 p31

# input line 39: register int64 p32

# input line 40: register int64 p33

# input line 41: register int64 q0

# input line 42: register int64 q1

# input line 43: register int64 q2

# input line 44: register int64 q3

# input line 45: register int64 k00

# input line 46: register int64 k01

# input line 47: register int64 k02

# input line 48: register int64 k03

# input line 49: register int64 k10

# input line 50: register int64 k11

# input line 51: register int64 k12

# input line 52: register int64 k13

# input line 53: register int64 k20

# input line 54: register int64 k21

# input line 55: register int64 k22

# input line 56: register int64 k23

# input line 57: register int64 k30

# input line 58: register int64 k31

# input line 59: register int64 k32

# input line 60: register int64 k33

# input line 61: register int64 n00

# input line 62: register int64 n01

# input line 63: register int64 n02

# input line 64: register int64 n03

# input line 65: register int64 n10

# input line 66: register int64 n11

# input line 67: register int64 n12

# input line 68: register int64 n13

# input line 69: register int64 n20

# input line 70: register int64 n21

# input line 71: register int64 n22

# input line 72: register int64 n23

# input line 73: register int64 n30

# input line 74: register int64 n31

# input line 75: register int64 n32

# input line 76: register int64 n33

# input line 77: 

# input line 78: enter aes_sparc
.section ".text"
.align 32
.global aes_sparc
aes_sparc:
save %sp,-208,%sp

# input line 79: input out

# input line 80: input k

# input line 81: input n

# input line 82: 

# input line 83: table0 = "%hh(aes_sparc_constants+40)" * 1024
# table0!%l0 = %hh(aes_sparc_constants+40) * 1024
sethi %hh(aes_sparc_constants+40),%l0
# live registers: 4 int64, 0 double

# input line 84: byte3 = 4177920 * 1024
# byte3!%o5 = 4177920 * 1024
sethi 4177920,%o5
# live registers: 5 int64, 0 double

# input line 85: k30 = *(uchar *) (k + 12)
# k30!%l2 = *(uchar *) (k!%i1 + 12)
ldub [%i1+12],%l2
# live registers: 6 int64, 0 double

# input line 86: 

# input line 87: byte2 = (uint64) byte3 >> 8
# byte2!%o0 = (uint64) byte3!%o5 >> 8
srlx %o5,8,%o0
# live registers: 7 int64, 0 double

# input line 88: table0 |= "%hm(aes_sparc_constants+40)"
# table0#2!%l0 = table0!%l0 | %hm(aes_sparc_constants+40)
or %l0,%hm(aes_sparc_constants+40),%l0
# live registers: 7 int64, 0 double

# input line 89: k31 = *(uchar *) (k + 13)
# k31!%l3 = *(uchar *) (k!%i1 + 13)
ldub [%i1+13],%l3
# live registers: 8 int64, 0 double

# input line 90: 

# input line 91: byte1 = (uint64) byte3 >> 16
# byte1!%l4 = (uint64) byte3!%o5 >> 16
srlx %o5,16,%l4
# live registers: 9 int64, 0 double

# input line 92: e = "%lm(aes_sparc_constants+40)" * 1024
# e!%l1 = %lm(aes_sparc_constants+40) * 1024
sethi %lm(aes_sparc_constants+40),%l1
# live registers: 10 int64, 0 double

# input line 93: k32 = *(uchar *) (k + 14)
# k32!%l5 = *(uchar *) (k!%i1 + 14)
ldub [%i1+14],%l5
# live registers: 11 int64, 0 double

# input line 94: 

# input line 95: table0 <<= 32
# table0#3!%l0 = table0#2!%l0 << 32
sllx %l0,32,%l0
# live registers: 11 int64, 0 double

# input line 96: e |= "%lo(aes_sparc_constants+40)"
# e#2!%l1 = e!%l1 | %lo(aes_sparc_constants+40)
or %l1,%lo(aes_sparc_constants+40),%l1
# live registers: 11 int64, 0 double

# input line 97: k33 = *(uchar *) (k + 15)
# k33!%l6 = *(uchar *) (k!%i1 + 15)
ldub [%i1+15],%l6
# live registers: 12 int64, 0 double

# input line 98: 

# input line 99: k31 <<= 8
# k31#2!%l3 = k31!%l3 << 8
sllx %l3,8,%l3
# live registers: 12 int64, 0 double

# input line 100: table0 |= e
# table0#4!%o1 = table0#3!%l0 | e#2!%l1
or %l0,%l1,%o1
# live registers: 11 int64, 0 double

# input line 101: k00 = *(uchar *) (k + 0)
# k00!%l1 = *(uchar *) (k!%i1 + 0)
ldub [%i1+0],%l1
# live registers: 12 int64, 0 double

# input line 102: 

# input line 103: k32 <<= 16
# k32#2!%l5 = k32!%l5 << 16
sllx %l5,16,%l5
# live registers: 12 int64, 0 double

# input line 104: table1 = table0 + 1024
# table1!%o2 = table0#4!%o1 + 1024
add %o1,1024,%o2
# live registers: 13 int64, 0 double

# input line 105: 

# input line 106: x3 = k30 ^ k31
# x3!%l0 = k30!%l2 ^ k31#2!%l3
xor %l2,%l3,%l0
# live registers: 12 int64, 0 double

# input line 107: k01 = *(uchar *) (k + 1)
# k01!%l2 = *(uchar *) (k!%i1 + 1)
ldub [%i1+1],%l2
# live registers: 13 int64, 0 double

# input line 108: 

# input line 109: k33 <<= 24
# k33#2!%l3 = k33!%l6 << 24
sllx %l6,24,%l3
# live registers: 13 int64, 0 double

# input line 110: x3 ^= k32
# x3#2!%l0 = x3!%l0 ^ k32#2!%l5
xor %l0,%l5,%l0
# live registers: 12 int64, 0 double

# input line 111: k02 = *(uchar *) (k + 2)
# k02!%l5 = *(uchar *) (k!%i1 + 2)
ldub [%i1+2],%l5
# live registers: 13 int64, 0 double

# input line 112: 

# input line 113: table2 = table0 + 2048
# table2!%o3 = table0#4!%o1 + 2048
add %o1,2048,%o3
# live registers: 14 int64, 0 double

# input line 114: x3 ^= k33
# x3#3!%l3 = x3#2!%l0 ^ k33#2!%l3
xor %l0,%l3,%l3
# live registers: 13 int64, 0 double

# input line 115: k03 = *(uchar *) (k + 3)
# k03!%l6 = *(uchar *) (k!%i1 + 3)
ldub [%i1+3],%l6
# live registers: 14 int64, 0 double

# input line 116: 

# input line 117: k01 <<= 8
# k01#2!%l0 = k01!%l2 << 8
sllx %l2,8,%l0
# live registers: 14 int64, 0 double

# input line 118: table3 = table0 + 3072
# table3!%o4 = table0#4!%o1 + 3072
add %o1,3072,%o4
# live registers: 15 int64, 0 double

# input line 119: k10 = *(uchar *) (k + 4)
# k10!%l7 = *(uchar *) (k!%i1 + 4)
ldub [%i1+4],%l7
# live registers: 16 int64, 0 double

# input line 120: 

# input line 121: k02 <<= 16
# k02#2!%l2 = k02!%l5 << 16
sllx %l5,16,%l2
# live registers: 16 int64, 0 double

# input line 122: x0 = k00 ^ k01
# x0!%l0 = k00!%l1 ^ k01#2!%l0
xor %l1,%l0,%l0
# live registers: 15 int64, 0 double

# input line 123: k11 = *(uchar *) (k + 5)
# k11!%o7 = *(uchar *) (k!%i1 + 5)
ldub [%i1+5],%o7
# live registers: 16 int64, 0 double

# input line 124: 

# input line 125: k03 <<= 24
# k03#2!%l5 = k03!%l6 << 24
sllx %l6,24,%l5
# live registers: 16 int64, 0 double

# input line 126: x0 ^= k02
# x0#2!%l0 = x0!%l0 ^ k02#2!%l2
xor %l0,%l2,%l0
# live registers: 15 int64, 0 double

# input line 127: k12 = *(uchar *) (k + 6)
# k12!%l6 = *(uchar *) (k!%i1 + 6)
ldub [%i1+6],%l6
# live registers: 16 int64, 0 double

# input line 128: 

# input line 129: q0 = (uint64) x3 >> 6
# q0!%l1 = (uint64) x3#3!%l3 >> 6
srlx %l3,6,%l1
# live registers: 17 int64, 0 double

# input line 130: x0 ^= k03
# x0#3!%l0 = x0#2!%l0 ^ k03#2!%l5
xor %l0,%l5,%l0
# live registers: 16 int64, 0 double

# input line 131: k13 = *(uchar *) (k + 7)
# k13!%i3 = *(uchar *) (k!%i1 + 7)
ldub [%i1+7],%i3
# live registers: 17 int64, 0 double

# input line 132: 

# input line 133: k11 <<= 8
# k11#2!%l5 = k11!%o7 << 8
sllx %o7,8,%l5
# live registers: 17 int64, 0 double

# input line 134: q0 &= 1020
# q0#2!%l2 = q0!%l1 & 1020
and %l1,1020,%l2
# live registers: 17 int64, 0 double

# input line 135: k20 = *(uchar *) (k + 8)
# k20!%o7 = *(uchar *) (k!%i1 + 8)
ldub [%i1+8],%o7
# live registers: 18 int64, 0 double

# input line 136: 

# input line 137: k12 <<= 16
# k12#2!%l6 = k12!%l6 << 16
sllx %l6,16,%l6
# live registers: 18 int64, 0 double

# input line 138: x1 = k10 ^ k11
# x1!%l1 = k10!%l7 ^ k11#2!%l5
xor %l7,%l5,%l1
# live registers: 17 int64, 0 double

# input line 139: k21 = *(uchar *) (k + 9)
# k21!%i4 = *(uchar *) (k!%i1 + 9)
ldub [%i1+9],%i4
# live registers: 18 int64, 0 double

# input line 140: 

# input line 141: k13 <<= 24
# k13#2!%l7 = k13!%i3 << 24
sllx %i3,24,%l7
# live registers: 18 int64, 0 double

# input line 142: x1 ^= k12
# x1#2!%l1 = x1!%l1 ^ k12#2!%l6
xor %l1,%l6,%l1
# live registers: 17 int64, 0 double

# input line 143: k22 = *(uchar *) (k + 10)
# k22!%i3 = *(uchar *) (k!%i1 + 10)
ldub [%i1+10],%i3
# live registers: 18 int64, 0 double

# input line 144: 

# input line 145: q1 = (uint64) x3 >> 14
# q1!%l5 = (uint64) x3#3!%l3 >> 14
srlx %l3,14,%l5
# live registers: 19 int64, 0 double

# input line 146: x1 ^= k13
# x1#3!%l1 = x1#2!%l1 ^ k13#2!%l7
xor %l1,%l7,%l1
# live registers: 18 int64, 0 double

# input line 147: k23 = *(uchar *) (k + 11)
# k23!%i5 = *(uchar *) (k!%i1 + 11)
ldub [%i1+11],%i5
# live registers: 18 int64, 0 double

# input line 148: 

# input line 149: k21 <<= 8
# k21#2!%l7 = k21!%i4 << 8
sllx %i4,8,%l7
# live registers: 18 int64, 0 double

# input line 150: q1 &= 1020
# q1#2!%l6 = q1!%l5 & 1020
and %l5,1020,%l6
# live registers: 18 int64, 0 double

# input line 151: q0 = *(uint32 *) (table2 + q0)
# q0#3!%l5 = *(uint32 *) (table2!%o3 + q0#2!%l2)
lduw [%o3+%l2],%l5
# live registers: 18 int64, 0 double

# input line 152: 

# input line 153: k22 <<= 16
# k22#2!%i1 = k22!%i3 << 16
sllx %i3,16,%i1
# live registers: 18 int64, 0 double

# input line 154: x2 = k20 ^ k21
# x2!%l2 = k20!%o7 ^ k21#2!%l7
xor %o7,%l7,%l2
# live registers: 17 int64, 0 double

# input line 155: q1 = *(uint32 *) (table3 + q1)
# q1#3!%l6 = *(uint32 *) (table3!%o4 + q1#2!%l6)
lduw [%o4+%l6],%l6
# live registers: 17 int64, 0 double

# input line 156: 

# input line 157: k23 <<= 24
# k23#2!%o7 = k23!%i5 << 24
sllx %i5,24,%o7
# live registers: 17 int64, 0 double

# input line 158: x2 ^= k22
# x2#2!%l2 = x2!%l2 ^ k22#2!%i1
xor %l2,%i1,%l2
# live registers: 16 int64, 0 double

# input line 159: n00 = *(uchar *) (n + 0)
# n00!%i1 = *(uchar *) (n!%i2 + 0)
ldub [%i2+0],%i1
# live registers: 17 int64, 0 double

# input line 160: 

# input line 161: q2 = (uint64) x3 >> 22
# q2!%l7 = (uint64) x3#3!%l3 >> 22
srlx %l3,22,%l7
# live registers: 18 int64, 0 double

# input line 162: x2 ^= k23
# x2#3!%l2 = x2#2!%l2 ^ k23#2!%o7
xor %l2,%o7,%l2
# live registers: 17 int64, 0 double

# input line 163: n01 = *(uchar *) (n + 1)
# n01!%i5 = *(uchar *) (n!%i2 + 1)
ldub [%i2+1],%i5
# live registers: 18 int64, 0 double

# input line 164: 

# input line 165: q3 = x3 << 2
# q3!%o7 = x3#3!%l3 << 2
sllx %l3,2,%o7
# live registers: 19 int64, 0 double

# input line 166: q2 &= 1020
# q2#2!%l7 = q2!%l7 & 1020
and %l7,1020,%l7
# live registers: 19 int64, 0 double

# input line 167: n02 = *(uchar *) (n + 2)
# n02!%g1 = *(uchar *) (n!%i2 + 2)
ldub [%i2+2],%g1
# live registers: 20 int64, 0 double

# input line 168: 

# input line 169: q3 &= 1020
# q3#2!%o7 = q3!%o7 & 1020
and %o7,1020,%o7
# live registers: 20 int64, 0 double

# input line 170: q0 &= 255
# q0#4!%l5 = q0#3!%l5 & 255
and %l5,255,%l5
# live registers: 20 int64, 0 double

# input line 171: q2 = *(uint32 *) (table0 + q2)
# q2#3!%l7 = *(uint32 *) (table0#4!%o1 + q2#2!%l7)
lduw [%o1+%l7],%l7
# live registers: 20 int64, 0 double

# input line 172: 

# input line 173: q1 &= byte1
# q1#4!%l6 = q1#3!%l6 & byte1!%l4
and %l6,%l4,%l6
# live registers: 20 int64, 0 double

# input line 174: e = q0 ^ 1
# e#3!%l5 = q0#4!%l5 ^ 1
xor %l5,1,%l5
# live registers: 20 int64, 0 double

# input line 175: q3 = *(uint32 *) (table1 + q3)
# q3#3!%o7 = *(uint32 *) (table1!%o2 + q3#2!%o7)
lduw [%o2+%o7],%o7
# live registers: 20 int64, 0 double

# input line 176: 

# input line 177: q2 &= byte2
# q2#4!%l7 = q2#3!%l7 & byte2!%o0
and %l7,%o0,%l7
# live registers: 20 int64, 0 double

# input line 178: e ^= q1
# e#4!%l5 = e#3!%l5 ^ q1#4!%l6
xor %l5,%l6,%l5
# live registers: 19 int64, 0 double

# input line 179: spill byte1
# byte1@spill!spill0 = byte1!%l4
stx %l4,[%fp+2023]
# live registers: 18 int64, 0 double

# input line 180: 

# input line 181: q3 &= byte3
# q3#4!%i4 = q3#3!%o7 & byte3!%o5
and %o7,%o5,%i4
# live registers: 18 int64, 0 double

# input line 182: e ^= q2
# e#5!%i3 = e#4!%l5 ^ q2#4!%l7
xor %l5,%l7,%i3
# live registers: 17 int64, 0 double

# input line 183: n03 = *(uchar *) (n + 3)
# n03!%l7 = *(uchar *) (n!%i2 + 3)
ldub [%i2+3],%l7
# live registers: 18 int64, 0 double

# input line 184: 

# input line 185: n01 <<= 8
# n01#2!%l5 = n01!%i5 << 8
sllx %i5,8,%l5
# live registers: 18 int64, 0 double

# input line 186: y0 = x0 ^ n00
# y0!%l4 = x0#3!%l0 ^ n00!%i1
xor %l0,%i1,%l4
# live registers: 18 int64, 0 double

# input line 187: n10 = *(uchar *) (n + 4)
# n10!%o7 = *(uchar *) (n!%i2 + 4)
ldub [%i2+4],%o7
# live registers: 19 int64, 0 double

# input line 188: 

# input line 189: n02 <<= 16
# n02#2!%l6 = n02!%g1 << 16
sllx %g1,16,%l6
# live registers: 19 int64, 0 double

# input line 190: y0 ^= n01
# y0#2!%l4 = y0!%l4 ^ n01#2!%l5
xor %l4,%l5,%l4
# live registers: 18 int64, 0 double

# input line 191: n11 = *(uchar *) (n + 5)
# n11!%i1 = *(uchar *) (n!%i2 + 5)
ldub [%i2+5],%i1
# live registers: 19 int64, 0 double

# input line 192: 

# input line 193: n03 <<= 24
# n03#2!%l5 = n03!%l7 << 24
sllx %l7,24,%l5
# live registers: 19 int64, 0 double

# input line 194: y0 ^= n02
# y0#3!%l4 = y0#2!%l4 ^ n02#2!%l6
xor %l4,%l6,%l4
# live registers: 18 int64, 0 double

# input line 195: n12 = *(uchar *) (n + 6)
# n12!%l7 = *(uchar *) (n!%i2 + 6)
ldub [%i2+6],%l7
# live registers: 19 int64, 0 double

# input line 196: 

# input line 197: y0 ^= n03
# y0#4!%l4 = y0#3!%l4 ^ n03#2!%l5
xor %l4,%l5,%l4
# live registers: 18 int64, 0 double

# input line 198: n13 = *(uchar *) (n + 7)
# n13!%i5 = *(uchar *) (n!%i2 + 7)
ldub [%i2+7],%i5
# live registers: 19 int64, 0 double

# input line 199: 

# input line 200: n11 <<= 8
# n11#2!%l6 = n11!%i1 << 8
sllx %i1,8,%l6
# live registers: 19 int64, 0 double

# input line 201: y1 = x1 ^ n10
# y1!%l5 = x1#3!%l1 ^ n10!%o7
xor %l1,%o7,%l5
# live registers: 19 int64, 0 double

# input line 202: n20 = *(uchar *) (n + 8)
# n20!%o7 = *(uchar *) (n!%i2 + 8)
ldub [%i2+8],%o7
# live registers: 20 int64, 0 double

# input line 203: 

# input line 204: n12 <<= 16
# n12#2!%l7 = n12!%l7 << 16
sllx %l7,16,%l7
# live registers: 20 int64, 0 double

# input line 205: y1 ^= n11
# y1#2!%l5 = y1!%l5 ^ n11#2!%l6
xor %l5,%l6,%l5
# live registers: 19 int64, 0 double

# input line 206: n21 = *(uchar *) (n + 9)
# n21!%i1 = *(uchar *) (n!%i2 + 9)
ldub [%i2+9],%i1
# live registers: 20 int64, 0 double

# input line 207: 

# input line 208: n13 <<= 24
# n13#2!%l6 = n13!%i5 << 24
sllx %i5,24,%l6
# live registers: 20 int64, 0 double

# input line 209: y1 ^= n12
# y1#3!%l5 = y1#2!%l5 ^ n12#2!%l7
xor %l5,%l7,%l5
# live registers: 19 int64, 0 double

# input line 210: n22 = *(uchar *) (n + 10)
# n22!%i5 = *(uchar *) (n!%i2 + 10)
ldub [%i2+10],%i5
# live registers: 20 int64, 0 double

# input line 211: 

# input line 212: y1 ^= n13
# y1#4!%l5 = y1#3!%l5 ^ n13#2!%l6
xor %l5,%l6,%l5
# live registers: 19 int64, 0 double

# input line 213: n23 = *(uchar *) (n + 11)
# n23!%g1 = *(uchar *) (n!%i2 + 11)
ldub [%i2+11],%g1
# live registers: 20 int64, 0 double

# input line 214: 

# input line 215: n21 <<= 8
# n21#2!%l7 = n21!%i1 << 8
sllx %i1,8,%l7
# live registers: 20 int64, 0 double

# input line 216: y2 = x2 ^ n20
# y2!%l6 = x2#3!%l2 ^ n20!%o7
xor %l2,%o7,%l6
# live registers: 20 int64, 0 double

# input line 217: n30 = *(uchar *) (n + 12)
# n30!%i1 = *(uchar *) (n!%i2 + 12)
ldub [%i2+12],%i1
# live registers: 21 int64, 0 double

# input line 218: 

# input line 219: n22 <<= 16
# n22#2!%o7 = n22!%i5 << 16
sllx %i5,16,%o7
# live registers: 21 int64, 0 double

# input line 220: y2 ^= n21
# y2#2!%l6 = y2!%l6 ^ n21#2!%l7
xor %l6,%l7,%l6
# live registers: 20 int64, 0 double

# input line 221: n31 = *(uchar *) (n + 13)
# n31!%i5 = *(uchar *) (n!%i2 + 13)
ldub [%i2+13],%i5
# live registers: 21 int64, 0 double

# input line 222: 

# input line 223: n23 <<= 24
# n23#2!%l7 = n23!%g1 << 24
sllx %g1,24,%l7
# live registers: 21 int64, 0 double

# input line 224: y2 ^= n22
# y2#3!%l6 = y2#2!%l6 ^ n22#2!%o7
xor %l6,%o7,%l6
# live registers: 20 int64, 0 double

# input line 225: n32 = *(uchar *) (n + 14)
# n32!%g1 = *(uchar *) (n!%i2 + 14)
ldub [%i2+14],%g1
# live registers: 21 int64, 0 double

# input line 226: 

# input line 227: y2 ^= n23
# y2#4!%l6 = y2#3!%l6 ^ n23#2!%l7
xor %l6,%l7,%l6
# live registers: 20 int64, 0 double

# input line 228: n33 = *(uchar *) (n + 15)
# n33!%i2 = *(uchar *) (n!%i2 + 15)
ldub [%i2+15],%i2
# live registers: 20 int64, 0 double

# input line 229: 

# input line 230: n31 <<= 8
# n31#2!%o7 = n31!%i5 << 8
sllx %i5,8,%o7
# live registers: 20 int64, 0 double

# input line 231: y3 = x3 ^ n30
# y3!%l7 = x3#3!%l3 ^ n30!%i1
xor %l3,%i1,%l7
# live registers: 20 int64, 0 double

# input line 232: spill byte2
# byte2@spill!spill8 = byte2!%o0
stx %o0,[%fp+2015]
# live registers: 19 int64, 0 double

# input line 233: 

# input line 234: n32 <<= 16
# n32#2!%o0 = n32!%g1 << 16
sllx %g1,16,%o0
# live registers: 19 int64, 0 double

# input line 235: y3 ^= n31
# y3#2!%l7 = y3!%l7 ^ n31#2!%o7
xor %l7,%o7,%l7
# live registers: 18 int64, 0 double

# input line 236: spill byte3
# byte3@spill!spill16 = byte3!%o5
stx %o5,[%fp+2007]
# live registers: 17 int64, 0 double

# input line 237: 

# input line 238: n33 <<= 24
# n33#2!%o5 = n33!%i2 << 24
sllx %i2,24,%o5
# live registers: 17 int64, 0 double

# input line 239: y3 ^= n32
# y3#3!%l7 = y3#2!%l7 ^ n32#2!%o0
xor %l7,%o0,%l7
# live registers: 16 int64, 0 double

# input line 240: 

# input line 241: y3 ^= n33
# y3#4!%l7 = y3#3!%l7 ^ n33#2!%o5
xor %l7,%o5,%l7
# live registers: 15 int64, 0 double

# input line 242: loop4 = "-36"
# loop4!%o0 = -36
add %g0,-36,%o0
# live registers: 16 int64, 0 double

# input line 243: 

# input line 244: 

# input line 245: mainloop
._mainloop:

# input line 246:   p00 = y0 << 2
# p00!%o7 = y0#4!%l4 << 2
sllx %l4,2,%o7
# live registers: 17 int64, 0 double

# input line 247:   e ^= q3
# e#6!%o5 = e#5!%i3 ^ q3#4!%i4
xor %i3,%i4,%o5
# live registers: 16 int64, 0 double

# input line 248: 

# input line 249:   p01 = (uint64) y0 >> 6
# p01!%i1 = (uint64) y0#4!%l4 >> 6
srlx %l4,6,%i1
# live registers: 17 int64, 0 double

# input line 250:   x0 ^= e
# x0#3!%l0 = x0#3!%l0 ^ e#6!%o5
xor %l0,%o5,%l0
# live registers: 16 int64, 0 double

# input line 251: 

# input line 252:   p02 = (uint64) y0 >> 14
# p02!%i2 = (uint64) y0#4!%l4 >> 14
srlx %l4,14,%i2
# live registers: 17 int64, 0 double

# input line 253:   x1 ^= x0
# x1#3!%l1 = x1#3!%l1 ^ x0#3!%l0
xor %l1,%l0,%l1
# live registers: 17 int64, 0 double

# input line 254: 

# input line 255:   p03 = (uint64) y0 >> 22
# p03!%i3 = (uint64) y0#4!%l4 >> 22
srlx %l4,22,%i3
# live registers: 17 int64, 0 double

# input line 256:   p00 &= 1020
# p00#2!%l4 = p00!%o7 & 1020
and %o7,1020,%l4
# live registers: 17 int64, 0 double

# input line 257: 

# input line 258:   p01 &= 1020
# p01#2!%o5 = p01!%i1 & 1020
and %i1,1020,%o5
# live registers: 17 int64, 0 double

# input line 259:   x2 ^= x1
# x2#3!%l2 = x2#3!%l2 ^ x1#3!%l1
xor %l2,%l1,%l2
# live registers: 17 int64, 0 double

# input line 260:   p00 = *(uint32 *) (table0 + p00)
# p00#3!%l4 = *(uint32 *) (table0#4!%o1 + p00#2!%l4)
lduw [%o1+%l4],%l4
# live registers: 17 int64, 0 double

# input line 261: 

# input line 262:   x3 ^= x2
# x3#3!%l3 = x3#3!%l3 ^ x2#3!%l2
xor %l3,%l2,%l3
# live registers: 17 int64, 0 double

# input line 263:   p02 &= 1020
# p02#2!%o7 = p02!%i2 & 1020
and %i2,1020,%o7
# live registers: 17 int64, 0 double

# input line 264:   p01 = *(uint32 *) (table1 + p01)
# p01#3!%o5 = *(uint32 *) (table1!%o2 + p01#2!%o5)
lduw [%o2+%o5],%o5
# live registers: 17 int64, 0 double

# input line 265: 

# input line 266:   p03 &= 1020
# p03#2!%i1 = p03!%i3 & 1020
and %i3,1020,%i1
# live registers: 17 int64, 0 double

# input line 267:   z0 = x0 ^ p00
# z0!%i2 = x0#3!%l0 ^ p00#3!%l4
xor %l0,%l4,%i2
# live registers: 17 int64, 0 double

# input line 268:   p02 = *(uint32 *) (table2 + p02)
# p02#3!%l4 = *(uint32 *) (table2!%o3 + p02#2!%o7)
lduw [%o3+%o7],%l4
# live registers: 17 int64, 0 double

# input line 269: 

# input line 270:   p10 = y1 << 2
# p10!%i4 = y1#4!%l5 << 2
sllx %l5,2,%i4
# live registers: 18 int64, 0 double

# input line 271:   z3 = x3 ^ p01
# z3!%i3 = x3#3!%l3 ^ p01#3!%o5
xor %l3,%o5,%i3
# live registers: 18 int64, 0 double

# input line 272:   p03 = *(uint32 *) (table3 + p03)
# p03#3!%o5 = *(uint32 *) (table3!%o4 + p03#2!%i1)
lduw [%o4+%i1],%o5
# live registers: 18 int64, 0 double

# input line 273: 

# input line 274:   p11 = (uint64) y1 >> 6
# p11!%i5 = (uint64) y1#4!%l5 >> 6
srlx %l5,6,%i5
# live registers: 19 int64, 0 double

# input line 275:   z2 = x2 ^ p02
# z2!%o7 = x2#3!%l2 ^ p02#3!%l4
xor %l2,%l4,%o7
# live registers: 19 int64, 0 double

# input line 276: 

# input line 277:   p10 &= 1020
# p10#2!%i1 = p10!%i4 & 1020
and %i4,1020,%i1
# live registers: 19 int64, 0 double

# input line 278:   z1 = x1 ^ p03
# z1!%l4 = x1#3!%l1 ^ p03#3!%o5
xor %l1,%o5,%l4
# live registers: 19 int64, 0 double

# input line 279: 

# input line 280:   p12 = (uint64) y1 >> 14
# p12!%g1 = (uint64) y1#4!%l5 >> 14
srlx %l5,14,%g1
# live registers: 20 int64, 0 double

# input line 281:   p11 &= 1020
# p11#2!%i4 = p11!%i5 & 1020
and %i5,1020,%i4
# live registers: 20 int64, 0 double

# input line 282:   p10 = *(uint32 *) (table0 + p10)
# p10#3!%o5 = *(uint32 *) (table0#4!%o1 + p10#2!%i1)
lduw [%o1+%i1],%o5
# live registers: 20 int64, 0 double

# input line 283: 

# input line 284:   p13 = (uint64) y1 >> 22
# p13!%l5 = (uint64) y1#4!%l5 >> 22
srlx %l5,22,%l5
# live registers: 20 int64, 0 double

# input line 285:   p12 &= 1020
# p12#2!%i5 = p12!%g1 & 1020
and %g1,1020,%i5
# live registers: 20 int64, 0 double

# input line 286:   p11 = *(uint32 *) (table1 + p11)
# p11#3!%i1 = *(uint32 *) (table1!%o2 + p11#2!%i4)
lduw [%o2+%i4],%i1
# live registers: 20 int64, 0 double

# input line 287: 

# input line 288:   p13 &= 1020
# p13#2!%i4 = p13!%l5 & 1020
and %l5,1020,%i4
# live registers: 20 int64, 0 double

# input line 289:   z1 ^= p10
# z1#2!%l5 = z1!%l4 ^ p10#3!%o5
xor %l4,%o5,%l5
# live registers: 19 int64, 0 double

# input line 290:   p12 = *(uint32 *) (table2 + p12)
# p12#3!%o5 = *(uint32 *) (table2!%o3 + p12#2!%i5)
lduw [%o3+%i5],%o5
# live registers: 19 int64, 0 double

# input line 291: 

# input line 292:   p20 = y2 << 2
# p20!%i5 = y2#4!%l6 << 2
sllx %l6,2,%i5
# live registers: 20 int64, 0 double

# input line 293:   z0 ^= p11
# z0#2!%l4 = z0!%i2 ^ p11#3!%i1
xor %i2,%i1,%l4
# live registers: 19 int64, 0 double

# input line 294:   p13 = *(uint32 *) (table3 + p13)
# p13#3!%i2 = *(uint32 *) (table3!%o4 + p13#2!%i4)
lduw [%o4+%i4],%i2
# live registers: 19 int64, 0 double

# input line 295: 

# input line 296:   p21 = (uint64) y2 >> 6
# p21!%i4 = (uint64) y2#4!%l6 >> 6
srlx %l6,6,%i4
# live registers: 20 int64, 0 double

# input line 297:   z3 ^= p12
# z3#2!%i1 = z3!%i3 ^ p12#3!%o5
xor %i3,%o5,%i1
# live registers: 19 int64, 0 double

# input line 298: 

# input line 299:   p22 = (uint64) y2 >> 14
# p22!%i3 = (uint64) y2#4!%l6 >> 14
srlx %l6,14,%i3
# live registers: 20 int64, 0 double

# input line 300:   z2 ^= p13
# z2#2!%o5 = z2!%o7 ^ p13#3!%i2
xor %o7,%i2,%o5
# live registers: 19 int64, 0 double

# input line 301: 

# input line 302:   p23 = (uint64) y2 >> 22
# p23!%g1 = (uint64) y2#4!%l6 >> 22
srlx %l6,22,%g1
# live registers: 19 int64, 0 double

# input line 303:   p20 &= 1020
# p20#2!%l6 = p20!%i5 & 1020
and %i5,1020,%l6
# live registers: 19 int64, 0 double

# input line 304: 

# input line 305:   p21 &= 1020
# p21#2!%o7 = p21!%i4 & 1020
and %i4,1020,%o7
# live registers: 19 int64, 0 double

# input line 306:   p22 &= 1020
# p22#2!%i2 = p22!%i3 & 1020
and %i3,1020,%i2
# live registers: 19 int64, 0 double

# input line 307:   p20 = *(uint32 *) (table0 + p20)
# p20#3!%l6 = *(uint32 *) (table0#4!%o1 + p20#2!%l6)
lduw [%o1+%l6],%l6
# live registers: 19 int64, 0 double

# input line 308: 

# input line 309:   p31 = (uint64) y3 >> 6
# p31!%i4 = (uint64) y3#4!%l7 >> 6
srlx %l7,6,%i4
# live registers: 20 int64, 0 double

# input line 310:   p23 &= 1020
# p23#2!%i3 = p23!%g1 & 1020
and %g1,1020,%i3
# live registers: 20 int64, 0 double

# input line 311:   p21 = *(uint32 *) (table1 + p21)
# p21#3!%o7 = *(uint32 *) (table1!%o2 + p21#2!%o7)
lduw [%o2+%o7],%o7
# live registers: 20 int64, 0 double

# input line 312: 

# input line 313:   z2 ^= p20
# z2#3!%l6 = z2#2!%o5 ^ p20#3!%l6
xor %o5,%l6,%l6
# live registers: 19 int64, 0 double

# input line 314:   p31 &= 1020
# p31#2!%i4 = p31!%i4 & 1020
and %i4,1020,%i4
# live registers: 19 int64, 0 double

# input line 315:   p22 = *(uint32 *) (table2 + p22)
# p22#3!%o5 = *(uint32 *) (table2!%o3 + p22#2!%i2)
lduw [%o3+%i2],%o5
# live registers: 19 int64, 0 double

# input line 316: 

# input line 317:   z1 ^= p21
# z1#3!%l5 = z1#2!%l5 ^ p21#3!%o7
xor %l5,%o7,%l5
# live registers: 18 int64, 0 double

# input line 318:   p23 = *(uint32 *) (table3 + p23)
# p23#3!%o7 = *(uint32 *) (table3!%o4 + p23#2!%i3)
lduw [%o4+%i3],%o7
# live registers: 18 int64, 0 double

# input line 319: 

# input line 320:   p32 = (uint64) y3 >> 14
# p32!%i3 = (uint64) y3#4!%l7 >> 14
srlx %l7,14,%i3
# live registers: 19 int64, 0 double

# input line 321:   z0 ^= p22
# z0#3!%l4 = z0#2!%l4 ^ p22#3!%o5
xor %l4,%o5,%l4
# live registers: 18 int64, 0 double

# input line 322:   p31 = *(uint32 *) (table1 + p31)
# p31#3!%i2 = *(uint32 *) (table1!%o2 + p31#2!%i4)
lduw [%o2+%i4],%i2
# live registers: 18 int64, 0 double

# input line 323: 

# input line 324:   p33 = (uint64) y3 >> 22
# p33!%i5 = (uint64) y3#4!%l7 >> 22
srlx %l7,22,%i5
# live registers: 19 int64, 0 double

# input line 325:   p32 &= 1020
# p32#2!%i4 = p32!%i3 & 1020
and %i3,1020,%i4
# live registers: 19 int64, 0 double

# input line 326:   unspill byte1
# byte1#2!%o5 = byte1@spill!spill0
ldx [%fp+2023],%o5
# live registers: 20 int64, 0 double

# input line 327: 

# input line 328:   p30 = y3 << 2
# p30!%i3 = y3#4!%l7 << 2
sllx %l7,2,%i3
# live registers: 20 int64, 0 double

# input line 329:   z3 ^= p23
# z3#3!%l7 = z3#2!%i1 ^ p23#3!%o7
xor %i1,%o7,%l7
# live registers: 19 int64, 0 double

# input line 330:   p32 = *(uint32 *) (table2 + p32)
# p32#3!%o7 = *(uint32 *) (table2!%o3 + p32#2!%i4)
lduw [%o3+%i4],%o7
# live registers: 19 int64, 0 double

# input line 331: 

# input line 332:   y2 = z2 ^ p31
# y2#4!%l6 = z2#3!%l6 ^ p31#3!%i2
xor %l6,%i2,%l6
# live registers: 18 int64, 0 double

# input line 333:   p33 &= 1020
# p33#2!%i1 = p33!%i5 & 1020
and %i5,1020,%i1
# live registers: 18 int64, 0 double

# input line 334:   e = *(uint32 *) (table0 + loop4)
# e#7!%i2 = *(uint32 *) (table0#4!%o1 + loop4!%o0)
lduw [%o1+%o0],%i2
# live registers: 19 int64, 0 double

# input line 335: 

# input line 336:   y1 = z1 ^ p32
# y1#4!%l5 = z1#3!%l5 ^ p32#3!%o7
xor %l5,%o7,%l5
# live registers: 18 int64, 0 double

# input line 337:   p30 &= 1020
# p30#2!%o7 = p30!%i3 & 1020
and %i3,1020,%o7
# live registers: 18 int64, 0 double

# input line 338:   p33 = *(uint32 *) (table3 + p33)
# p33#3!%i3 = *(uint32 *) (table3!%o4 + p33#2!%i1)
lduw [%o4+%i1],%i3
# live registers: 18 int64, 0 double

# input line 339: 

# input line 340:   q0 = (uint64) x3 >> 6
# q0#5!%i4 = (uint64) x3#3!%l3 >> 6
srlx %l3,6,%i4
# live registers: 19 int64, 0 double

# input line 341:   flags loop4 = loop4 + 4
# flags loop4!%o0 = loop4!%o0 + 4
addcc %o0,4,%o0
# live registers: 19 int64, 0 double

# input line 342:   p30 = *(uint32 *) (table0 + p30)
# p30#3!%i1 = *(uint32 *) (table0#4!%o1 + p30#2!%o7)
lduw [%o1+%o7],%i1
# live registers: 19 int64, 0 double

# input line 343: 

# input line 344:   q1 = (uint64) x3 >> 14
# q1#5!%i5 = (uint64) x3#3!%l3 >> 14
srlx %l3,14,%i5
# live registers: 20 int64, 0 double

# input line 345:   y0 = z0 ^ p33
# y0#4!%l4 = z0#3!%l4 ^ p33#3!%i3
xor %l4,%i3,%l4
# live registers: 19 int64, 0 double

# input line 346:   unspill byte2
# byte2#2!%o7 = byte2@spill!spill8
ldx [%fp+2015],%o7
# live registers: 20 int64, 0 double

# input line 347: 

# input line 348:   q0 &= 1020
# q0#6!%i3 = q0#5!%i4 & 1020
and %i4,1020,%i3
# live registers: 20 int64, 0 double

# input line 349:   y3 = z3 ^ p30
# y3#4!%l7 = z3#3!%l7 ^ p30#3!%i1
xor %l7,%i1,%l7
# live registers: 19 int64, 0 double

# input line 350:   unspill byte3
# byte3#2!%i1 = byte3@spill!spill16
ldx [%fp+2007],%i1
# live registers: 20 int64, 0 double

# input line 351: 

# input line 352:   q2 = (uint64) x3 >> 22
# q2#5!%g1 = (uint64) x3#3!%l3 >> 22
srlx %l3,22,%g1
# live registers: 21 int64, 0 double

# input line 353:   q1 &= 1020
# q1#6!%i4 = q1#5!%i5 & 1020
and %i5,1020,%i4
# live registers: 21 int64, 0 double

# input line 354:   q0 = *(uint32 *) (table2 + q0)
# q0#7!%i3 = *(uint32 *) (table2!%o3 + q0#6!%i3)
lduw [%o3+%i3],%i3
# live registers: 21 int64, 0 double

# input line 355: 

# input line 356:   q3 = x3 << 2
# q3#5!%g4 = x3#3!%l3 << 2
sllx %l3,2,%g4
# live registers: 22 int64, 0 double

# input line 357:   q2 &= 1020
# q2#6!%i5 = q2#5!%g1 & 1020
and %g1,1020,%i5
# live registers: 22 int64, 0 double

# input line 358:   q1 = *(uint32 *) (table3 + q1)
# q1#7!%i4 = *(uint32 *) (table3!%o4 + q1#6!%i4)
lduw [%o4+%i4],%i4
# live registers: 22 int64, 0 double

# input line 359: 

# input line 360:   q0 &= 255
# q0#8!%i3 = q0#7!%i3 & 255
and %i3,255,%i3
# live registers: 22 int64, 0 double

# input line 361:   q3 &= 1020
# q3#6!%g1 = q3#5!%g4 & 1020
and %g4,1020,%g1
# live registers: 22 int64, 0 double

# input line 362:   q2 = *(uint32 *) (table0 + q2)
# q2#7!%i5 = *(uint32 *) (table0#4!%o1 + q2#6!%i5)
lduw [%o1+%i5],%i5
# live registers: 22 int64, 0 double

# input line 363: 

# input line 364:   e ^= q0
# e#8!%i2 = e#7!%i2 ^ q0#8!%i3
xor %i2,%i3,%i2
# live registers: 21 int64, 0 double

# input line 365:   q1 &= byte1
# q1#8!%i3 = q1#7!%i4 & byte1#2!%o5
and %i4,%o5,%i3
# live registers: 21 int64, 0 double

# input line 366:   q3 = *(uint32 *) (table1 + q3)
# q3#7!%i4 = *(uint32 *) (table1!%o2 + q3#6!%g1)
lduw [%o2+%g1],%i4
# live registers: 21 int64, 0 double

# input line 367: 

# input line 368:   e ^= q1
# e#9!%i2 = e#8!%i2 ^ q1#8!%i3
xor %i2,%i3,%i2
# live registers: 20 int64, 0 double

# input line 369:   q2 &= byte2
# q2#8!%i3 = q2#7!%i5 & byte2#2!%o7
and %i5,%o7,%i3
# live registers: 20 int64, 0 double

# input line 370: 

# input line 371:   e ^= q2
# e#5!%i3 = e#9!%i2 ^ q2#8!%i3
xor %i2,%i3,%i3
# live registers: 19 int64, 0 double

# input line 372:   q3 &= byte3
# branch swapped from next instruction
bne,pt %xcc,._mainloop
# live registers: 19 int64, 0 double

# input line 373: goto mainloop if !=
# q3#4!%i4 = q3#7!%i4 & byte3#2!%i1
and %i4,%i1,%i4

# input line 374: 

# input line 375: 

# input line 376: p00 = y0 << 2
# p00#4!%i2 = y0#4!%l4 << 2
sllx %l4,2,%i2
# live registers: 19 int64, 0 double

# input line 377: e ^= q3
# e#10!%o0 = e#5!%i3 ^ q3#4!%i4
xor %i3,%i4,%o0
# live registers: 18 int64, 0 double

# input line 378: 

# input line 379: p01 = (uint64) y0 >> 6
# p01#4!%i3 = (uint64) y0#4!%l4 >> 6
srlx %l4,6,%i3
# live registers: 19 int64, 0 double

# input line 380: x0 ^= e
# x0#4!%l0 = x0#3!%l0 ^ e#10!%o0
xor %l0,%o0,%l0
# live registers: 18 int64, 0 double

# input line 381: 

# input line 382: p02 = (uint64) y0 >> 14
# p02#4!%i4 = (uint64) y0#4!%l4 >> 14
srlx %l4,14,%i4
# live registers: 19 int64, 0 double

# input line 383: x1 ^= x0
# x1#4!%l1 = x1#3!%l1 ^ x0#4!%l0
xor %l1,%l0,%l1
# live registers: 19 int64, 0 double

# input line 384: 

# input line 385: p03 = (uint64) y0 >> 22
# p03#4!%i5 = (uint64) y0#4!%l4 >> 22
srlx %l4,22,%i5
# live registers: 19 int64, 0 double

# input line 386: p00 &= 1020
# p00#5!%l4 = p00#4!%i2 & 1020
and %i2,1020,%l4
# live registers: 19 int64, 0 double

# input line 387: 

# input line 388: p01 &= 1020
# p01#5!%o0 = p01#4!%i3 & 1020
and %i3,1020,%o0
# live registers: 19 int64, 0 double

# input line 389: x2 ^= x1
# x2#4!%l2 = x2#3!%l2 ^ x1#4!%l1
xor %l2,%l1,%l2
# live registers: 19 int64, 0 double

# input line 390: p00 = *(uint32 *) (table2 + p00)
# p00#6!%l4 = *(uint32 *) (table2!%o3 + p00#5!%l4)
lduw [%o3+%l4],%l4
# live registers: 19 int64, 0 double

# input line 391: 

# input line 392: p02 &= 1020
# p02#5!%i2 = p02#4!%i4 & 1020
and %i4,1020,%i2
# live registers: 19 int64, 0 double

# input line 393: x3 ^= x2
# x3#4!%l3 = x3#3!%l3 ^ x2#4!%l2
xor %l3,%l2,%l3
# live registers: 19 int64, 0 double

# input line 394: p01 = *(uint32 *) (table3 + p01)
# p01#6!%o0 = *(uint32 *) (table3!%o4 + p01#5!%o0)
lduw [%o4+%o0],%o0
# live registers: 19 int64, 0 double

# input line 395: 

# input line 396: p03 &= 1020
# p03#5!%i3 = p03#4!%i5 & 1020
and %i5,1020,%i3
# live registers: 19 int64, 0 double

# input line 397: p00 &= 255
# p00#7!%l4 = p00#6!%l4 & 255
and %l4,255,%l4
# live registers: 19 int64, 0 double

# input line 398: p02 = *(uint32 *) (table0 + p02)
# p02#6!%i2 = *(uint32 *) (table0#4!%o1 + p02#5!%i2)
lduw [%o1+%i2],%i2
# live registers: 19 int64, 0 double

# input line 399: 

# input line 400: p01 &= byte1
# p01#7!%o0 = p01#6!%o0 & byte1#2!%o5
and %o0,%o5,%o0
# live registers: 19 int64, 0 double

# input line 401: z0 = x0 ^ p00
# z0#4!%i4 = x0#4!%l0 ^ p00#7!%l4
xor %l0,%l4,%i4
# live registers: 18 int64, 0 double

# input line 402: p03 = *(uint32 *) (table1 + p03)
# p03#6!%l4 = *(uint32 *) (table1!%o2 + p03#5!%i3)
lduw [%o2+%i3],%l4
# live registers: 18 int64, 0 double

# input line 403: 

# input line 404: p10 = y1 << 2
# p10#4!%i3 = y1#4!%l5 << 2
sllx %l5,2,%i3
# live registers: 19 int64, 0 double

# input line 405: p02 &= byte2
# p02#7!%l0 = p02#6!%i2 & byte2#2!%o7
and %i2,%o7,%l0
# live registers: 19 int64, 0 double

# input line 406: 

# input line 407: p11 = (uint64) y1 >> 6
# p11#4!%i5 = (uint64) y1#4!%l5 >> 6
srlx %l5,6,%i5
# live registers: 20 int64, 0 double

# input line 408: p03 &= byte3
# p03#7!%l4 = p03#6!%l4 & byte3#2!%i1
and %l4,%i1,%l4
# live registers: 20 int64, 0 double

# input line 409: 

# input line 410: p12 = (uint64) y1 >> 14
# p12#4!%g1 = (uint64) y1#4!%l5 >> 14
srlx %l5,14,%g1
# live registers: 21 int64, 0 double

# input line 411: p10 &= 1020
# p10#5!%i2 = p10#4!%i3 & 1020
and %i3,1020,%i2
# live registers: 21 int64, 0 double

# input line 412: 

# input line 413: p13 = (uint64) y1 >> 22
# p13#4!%g4 = (uint64) y1#4!%l5 >> 22
srlx %l5,22,%g4
# live registers: 21 int64, 0 double

# input line 414: p11 &= 1020
# p11#5!%i3 = p11#4!%i5 & 1020
and %i5,1020,%i3
# live registers: 21 int64, 0 double

# input line 415: p10 = *(uint32 *) (table2 + p10)
# p10#6!%l5 = *(uint32 *) (table2!%o3 + p10#5!%i2)
lduw [%o3+%i2],%l5
# live registers: 21 int64, 0 double

# input line 416: 

# input line 417: p12 &= 1020
# p12#5!%i2 = p12#4!%g1 & 1020
and %g1,1020,%i2
# live registers: 21 int64, 0 double

# input line 418: z3 = x3 ^ p01
# z3#4!%l3 = x3#4!%l3 ^ p01#7!%o0
xor %l3,%o0,%l3
# live registers: 20 int64, 0 double

# input line 419: p11 = *(uint32 *) (table3 + p11)
# p11#6!%o0 = *(uint32 *) (table3!%o4 + p11#5!%i3)
lduw [%o4+%i3],%o0
# live registers: 20 int64, 0 double

# input line 420: 

# input line 421: p10 &= 255
# p10#7!%l5 = p10#6!%l5 & 255
and %l5,255,%l5
# live registers: 20 int64, 0 double

# input line 422: p13 &= 1020
# p13#5!%i3 = p13#4!%g4 & 1020
and %g4,1020,%i3
# live registers: 20 int64, 0 double

# input line 423: p12 = *(uint32 *) (table0 + p12)
# p12#6!%i2 = *(uint32 *) (table0#4!%o1 + p12#5!%i2)
lduw [%o1+%i2],%i2
# live registers: 20 int64, 0 double

# input line 424: 

# input line 425: z2 = x2 ^ p02
# z2#4!%l2 = x2#4!%l2 ^ p02#7!%l0
xor %l2,%l0,%l2
# live registers: 19 int64, 0 double

# input line 426: p11 &= byte1
# p11#7!%o0 = p11#6!%o0 & byte1#2!%o5
and %o0,%o5,%o0
# live registers: 19 int64, 0 double

# input line 427: p13 = *(uint32 *) (table1 + p13)
# p13#6!%i3 = *(uint32 *) (table1!%o2 + p13#5!%i3)
lduw [%o2+%i3],%i3
# live registers: 19 int64, 0 double

# input line 428: 

# input line 429: p20 = y2 << 2
# p20#4!%i5 = y2#4!%l6 << 2
sllx %l6,2,%i5
# live registers: 20 int64, 0 double

# input line 430: z1 = x1 ^ p03
# z1#4!%l0 = x1#4!%l1 ^ p03#7!%l4
xor %l1,%l4,%l0
# live registers: 19 int64, 0 double

# input line 431: 

# input line 432: p21 = (uint64) y2 >> 6
# p21#4!%g1 = (uint64) y2#4!%l6 >> 6
srlx %l6,6,%g1
# live registers: 20 int64, 0 double

# input line 433: p12 &= byte2
# p12#7!%l4 = p12#6!%i2 & byte2#2!%o7
and %i2,%o7,%l4
# live registers: 20 int64, 0 double

# input line 434: 

# input line 435: p22 = (uint64) y2 >> 14
# p22#4!%g4 = (uint64) y2#4!%l6 >> 14
srlx %l6,14,%g4
# live registers: 21 int64, 0 double

# input line 436: p13 &= byte3
# p13#7!%i2 = p13#6!%i3 & byte3#2!%i1
and %i3,%i1,%i2
# live registers: 21 int64, 0 double

# input line 437: 

# input line 438: p23 = (uint64) y2 >> 22
# p23#4!%i3 = (uint64) y2#4!%l6 >> 22
srlx %l6,22,%i3
# live registers: 21 int64, 0 double

# input line 439: z1 ^= p10
# z1#5!%l1 = z1#4!%l0 ^ p10#7!%l5
xor %l0,%l5,%l1
# live registers: 20 int64, 0 double

# input line 440: 

# input line 441: p20 &= 1020
# p20#5!%l5 = p20#4!%i5 & 1020
and %i5,1020,%l5
# live registers: 20 int64, 0 double

# input line 442: z0 ^= p11
# z0#5!%l0 = z0#4!%i4 ^ p11#7!%o0
xor %i4,%o0,%l0
# live registers: 19 int64, 0 double

# input line 443: 

# input line 444: p21 &= 1020
# p21#5!%l6 = p21#4!%g1 & 1020
and %g1,1020,%l6
# live registers: 19 int64, 0 double

# input line 445: z3 ^= p12
# z3#5!%l4 = z3#4!%l3 ^ p12#7!%l4
xor %l3,%l4,%l4
# live registers: 18 int64, 0 double

# input line 446: p20 = *(uint32 *) (table2 + p20)
# p20#6!%l3 = *(uint32 *) (table2!%o3 + p20#5!%l5)
lduw [%o3+%l5],%l3
# live registers: 18 int64, 0 double

# input line 447: 

# input line 448: p22 &= 1020
# p22#5!%o0 = p22#4!%g4 & 1020
and %g4,1020,%o0
# live registers: 18 int64, 0 double

# input line 449: z2 ^= p13
# z2#5!%l2 = z2#4!%l2 ^ p13#7!%i2
xor %l2,%i2,%l2
# live registers: 17 int64, 0 double

# input line 450: p21 = *(uint32 *) (table3 + p21)
# p21#6!%l5 = *(uint32 *) (table3!%o4 + p21#5!%l6)
lduw [%o4+%l6],%l5
# live registers: 17 int64, 0 double

# input line 451: 

# input line 452: p23 &= 1020
# p23#5!%i2 = p23#4!%i3 & 1020
and %i3,1020,%i2
# live registers: 17 int64, 0 double

# input line 453: p20 &= 255
# p20#7!%l3 = p20#6!%l3 & 255
and %l3,255,%l3
# live registers: 17 int64, 0 double

# input line 454: p22 = *(uint32 *) (table0 + p22)
# p22#6!%l6 = *(uint32 *) (table0#4!%o1 + p22#5!%o0)
lduw [%o1+%o0],%l6
# live registers: 17 int64, 0 double

# input line 455: 

# input line 456: p21 &= byte1
# p21#7!%l5 = p21#6!%l5 & byte1#2!%o5
and %l5,%o5,%l5
# live registers: 17 int64, 0 double

# input line 457: z2 ^= p20
# z2#6!%l3 = z2#5!%l2 ^ p20#7!%l3
xor %l2,%l3,%l3
# live registers: 16 int64, 0 double

# input line 458: p23 = *(uint32 *) (table1 + p23)
# p23#6!%l2 = *(uint32 *) (table1!%o2 + p23#5!%i2)
lduw [%o2+%i2],%l2
# live registers: 16 int64, 0 double

# input line 459: 

# input line 460: p30 = y3 << 2
# p30#4!%i2 = y3#4!%l7 << 2
sllx %l7,2,%i2
# live registers: 17 int64, 0 double

# input line 461: p22 &= byte2
# p22#7!%l6 = p22#6!%l6 & byte2#2!%o7
and %l6,%o7,%l6
# live registers: 17 int64, 0 double

# input line 462: 

# input line 463: p31 = (uint64) y3 >> 6
# p31#4!%i3 = (uint64) y3#4!%l7 >> 6
srlx %l7,6,%i3
# live registers: 18 int64, 0 double

# input line 464: p23 &= byte3
# p23#7!%o0 = p23#6!%l2 & byte3#2!%i1
and %l2,%i1,%o0
# live registers: 18 int64, 0 double

# input line 465: 

# input line 466: p32 = (uint64) y3 >> 14
# p32#4!%i4 = (uint64) y3#4!%l7 >> 14
srlx %l7,14,%i4
# live registers: 19 int64, 0 double

# input line 467: z1 ^= p21
# z1#6!%l2 = z1#5!%l1 ^ p21#7!%l5
xor %l1,%l5,%l2
# live registers: 18 int64, 0 double

# input line 468: 

# input line 469: p33 = (uint64) y3 >> 22
# p33#4!%i5 = (uint64) y3#4!%l7 >> 22
srlx %l7,22,%i5
# live registers: 18 int64, 0 double

# input line 470: p30 &= 1020
# p30#5!%l5 = p30#4!%i2 & 1020
and %i2,1020,%l5
# live registers: 18 int64, 0 double

# input line 471: 

# input line 472: p31 &= 1020
# p31#5!%l7 = p31#4!%i3 & 1020
and %i3,1020,%l7
# live registers: 18 int64, 0 double

# input line 473: z0 ^= p22
# z0#6!%l1 = z0#5!%l0 ^ p22#7!%l6
xor %l0,%l6,%l1
# live registers: 17 int64, 0 double

# input line 474: p30 = *(uint32 *) (table2 + p30)
# p30#6!%l5 = *(uint32 *) (table2!%o3 + p30#5!%l5)
lduw [%o3+%l5],%l5
# live registers: 16 int64, 0 double

# input line 475: 

# input line 476: p32 &= 1020
# p32#5!%o3 = p32#4!%i4 & 1020
and %i4,1020,%o3
# live registers: 16 int64, 0 double

# input line 477: z3 ^= p23
# z3#6!%l0 = z3#5!%l4 ^ p23#7!%o0
xor %l4,%o0,%l0
# live registers: 15 int64, 0 double

# input line 478: p31 = *(uint32 *) (table3 + p31)
# p31#6!%l6 = *(uint32 *) (table3!%o4 + p31#5!%l7)
lduw [%o4+%l7],%l6
# live registers: 14 int64, 0 double

# input line 479: 

# input line 480: p33 &= 1020
# p33#5!%o0 = p33#4!%i5 & 1020
and %i5,1020,%o0
# live registers: 14 int64, 0 double

# input line 481: p30 &= 255
# p30#7!%l4 = p30#6!%l5 & 255
and %l5,255,%l4
# live registers: 14 int64, 0 double

# input line 482: p32 = *(uint32 *) (table0 + p32)
# p32#6!%l7 = *(uint32 *) (table0#4!%o1 + p32#5!%o3)
lduw [%o1+%o3],%l7
# live registers: 13 int64, 0 double

# input line 483: 

# input line 484: p31 &= byte1
# p31#7!%l5 = p31#6!%l6 & byte1#2!%o5
and %l6,%o5,%l5
# live registers: 12 int64, 0 double

# input line 485: y3 = z3 ^ p30
# y3#5!%l0 = z3#6!%l0 ^ p30#7!%l4
xor %l0,%l4,%l0
# live registers: 11 int64, 0 double

# input line 486: p33 = *(uint32 *) (table1 + p33)
# p33#6!%l6 = *(uint32 *) (table1!%o2 + p33#5!%o0)
lduw [%o2+%o0],%l6
# live registers: 10 int64, 0 double

# input line 487: 

# input line 488: *(uchar *) (out + 12) = y3
# *(uchar *) (out!%i0 + 12) = y3#5!%l0
stub %l0,[%i0+12]
# live registers: 10 int64, 0 double

# input line 489: y3 = (uint64) y3 >> 8
# y3#6!%l0 = (uint64) y3#5!%l0 >> 8
srlx %l0,8,%l0
# live registers: 10 int64, 0 double

# input line 490: *(uchar *) (out + 13) = y3
# *(uchar *) (out!%i0 + 13) = y3#6!%l0
stub %l0,[%i0+13]
# live registers: 10 int64, 0 double

# input line 491: y3 = (uint64) y3 >> 8
# y3#7!%l0 = (uint64) y3#6!%l0 >> 8
srlx %l0,8,%l0
# live registers: 10 int64, 0 double

# input line 492: *(uchar *) (out + 14) = y3
# *(uchar *) (out!%i0 + 14) = y3#7!%l0
stub %l0,[%i0+14]
# live registers: 10 int64, 0 double

# input line 493: y3 = (uint64) y3 >> 8
# y3#8!%l0 = (uint64) y3#7!%l0 >> 8
srlx %l0,8,%l0
# live registers: 10 int64, 0 double

# input line 494: *(uchar *) (out + 15) = y3
# *(uchar *) (out!%i0 + 15) = y3#8!%l0
stub %l0,[%i0+15]
# live registers: 9 int64, 0 double

# input line 495: 

# input line 496: p32 &= byte2
# p32#7!%l4 = p32#6!%l7 & byte2#2!%o7
and %l7,%o7,%l4
# live registers: 8 int64, 0 double

# input line 497: y2 = z2 ^ p31
# y2#5!%l0 = z2#6!%l3 ^ p31#7!%l5
xor %l3,%l5,%l0
# live registers: 7 int64, 0 double

# input line 498: 

# input line 499: *(uchar *) (out + 8) = y2
# *(uchar *) (out!%i0 + 8) = y2#5!%l0
stub %l0,[%i0+8]
# live registers: 7 int64, 0 double

# input line 500: y2 = (uint64) y2 >> 8
# y2#6!%l0 = (uint64) y2#5!%l0 >> 8
srlx %l0,8,%l0
# live registers: 7 int64, 0 double

# input line 501: *(uchar *) (out + 9) = y2
# *(uchar *) (out!%i0 + 9) = y2#6!%l0
stub %l0,[%i0+9]
# live registers: 7 int64, 0 double

# input line 502: y2 = (uint64) y2 >> 8
# y2#7!%l0 = (uint64) y2#6!%l0 >> 8
srlx %l0,8,%l0
# live registers: 7 int64, 0 double

# input line 503: *(uchar *) (out + 10) = y2
# *(uchar *) (out!%i0 + 10) = y2#7!%l0
stub %l0,[%i0+10]
# live registers: 7 int64, 0 double

# input line 504: y2 = (uint64) y2 >> 8
# y2#8!%l0 = (uint64) y2#7!%l0 >> 8
srlx %l0,8,%l0
# live registers: 7 int64, 0 double

# input line 505: *(uchar *) (out + 11) = y2
# *(uchar *) (out!%i0 + 11) = y2#8!%l0
stub %l0,[%i0+11]
# live registers: 6 int64, 0 double

# input line 506: 

# input line 507: p33 &= byte3
# p33#7!%l3 = p33#6!%l6 & byte3#2!%i1
and %l6,%i1,%l3
# live registers: 5 int64, 0 double

# input line 508: y1 = z1 ^ p32
# y1#5!%l0 = z1#6!%l2 ^ p32#7!%l4
xor %l2,%l4,%l0
# live registers: 4 int64, 0 double

# input line 509: 

# input line 510: *(uchar *) (out + 4) = y1
# *(uchar *) (out!%i0 + 4) = y1#5!%l0
stub %l0,[%i0+4]
# live registers: 4 int64, 0 double

# input line 511: y1 = (uint64) y1 >> 8
# y1#6!%l0 = (uint64) y1#5!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 512: *(uchar *) (out + 5) = y1
# *(uchar *) (out!%i0 + 5) = y1#6!%l0
stub %l0,[%i0+5]
# live registers: 4 int64, 0 double

# input line 513: y1 = (uint64) y1 >> 8
# y1#7!%l0 = (uint64) y1#6!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 514: *(uchar *) (out + 6) = y1
# *(uchar *) (out!%i0 + 6) = y1#7!%l0
stub %l0,[%i0+6]
# live registers: 4 int64, 0 double

# input line 515: y1 = (uint64) y1 >> 8
# y1#8!%l0 = (uint64) y1#7!%l0 >> 8
srlx %l0,8,%l0
# live registers: 4 int64, 0 double

# input line 516: *(uchar *) (out + 7) = y1
# *(uchar *) (out!%i0 + 7) = y1#8!%l0
stub %l0,[%i0+7]
# live registers: 3 int64, 0 double

# input line 517: 

# input line 518: y0 = z0 ^ p33
# y0#5!%l0 = z0#6!%l1 ^ p33#7!%l3
xor %l1,%l3,%l0
# live registers: 2 int64, 0 double

# input line 519: 

# input line 520: *(uchar *) (out + 0) = y0
# *(uchar *) (out!%i0 + 0) = y0#5!%l0
stub %l0,[%i0+0]
# live registers: 2 int64, 0 double

# input line 521: y0 = (uint64) y0 >> 8
# y0#6!%l0 = (uint64) y0#5!%l0 >> 8
srlx %l0,8,%l0
# live registers: 2 int64, 0 double

# input line 522: *(uchar *) (out + 1) = y0
# *(uchar *) (out!%i0 + 1) = y0#6!%l0
stub %l0,[%i0+1]
# live registers: 2 int64, 0 double

# input line 523: y0 = (uint64) y0 >> 8
# y0#7!%l0 = (uint64) y0#6!%l0 >> 8
srlx %l0,8,%l0
# live registers: 2 int64, 0 double

# input line 524: *(uchar *) (out + 2) = y0
# *(uchar *) (out!%i0 + 2) = y0#7!%l0
stub %l0,[%i0+2]
# live registers: 2 int64, 0 double

# input line 525: y0 = (uint64) y0 >> 8
# y0#8!%l0 = (uint64) y0#7!%l0 >> 8
srlx %l0,8,%l0
# live registers: 2 int64, 0 double

# input line 526: *(uchar *) (out + 3) = y0
# *(uchar *) (out!%i0 + 3) = y0#8!%l0
stub %l0,[%i0+3]
# live registers: 0 int64, 0 double

# input line 527: 

# input line 528: leave
ret
restore
