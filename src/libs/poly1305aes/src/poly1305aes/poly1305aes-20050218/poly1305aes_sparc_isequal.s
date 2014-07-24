# poly1305aes_sparc_isequal.s version 20050131
# D. J. Bernstein
# Public domain.

# translated by qhasm-sparc version 20050131

# input line 1: register int64 d

# input line 2: register int64 x

# input line 3: register int64 y

# input line 4: register int64 x0

# input line 5: register int64 x1

# input line 6: register int64 x2

# input line 7: register int64 x3

# input line 8: register int64 x4

# input line 9: register int64 x5

# input line 10: register int64 x6

# input line 11: register int64 x7

# input line 12: register int64 x8

# input line 13: register int64 x9

# input line 14: register int64 x10

# input line 15: register int64 x11

# input line 16: register int64 x12

# input line 17: register int64 x13

# input line 18: register int64 x14

# input line 19: register int64 x15

# input line 20: register int64 y0

# input line 21: register int64 y1

# input line 22: register int64 y2

# input line 23: register int64 y3

# input line 24: register int64 y4

# input line 25: register int64 y5

# input line 26: register int64 y6

# input line 27: register int64 y7

# input line 28: register int64 y8

# input line 29: register int64 y9

# input line 30: register int64 y10

# input line 31: register int64 y11

# input line 32: register int64 y12

# input line 33: register int64 y13

# input line 34: register int64 y14

# input line 35: register int64 y15

# input line 36: 

# input line 37: enter poly1305aes_sparc_isequal
.section ".text"
.align 32
.global poly1305aes_sparc_isequal
poly1305aes_sparc_isequal:
save %sp,-176,%sp

# input line 38: input x

# input line 39: input y

# input line 40: 

# input line 41:   x0 = *(uchar *) (x + 0)
# x0!%l0 = *(uchar *) (x!%i0 + 0)
ldub [%i0+0],%l0
# live registers: 3 int64, 0 double

# input line 42:   y0 = *(uchar *) (y + 0)
# y0!%l3 = *(uchar *) (y!%i1 + 0)
ldub [%i1+0],%l3
# live registers: 4 int64, 0 double

# input line 43:   x1 = *(uchar *) (x + 1)
# x1!%l1 = *(uchar *) (x!%i0 + 1)
ldub [%i0+1],%l1
# live registers: 5 int64, 0 double

# input line 44:   y1 = *(uchar *) (y + 1)
# y1!%l4 = *(uchar *) (y!%i1 + 1)
ldub [%i1+1],%l4
# live registers: 6 int64, 0 double

# input line 45:   x2 = *(uchar *) (x + 2)
# x2!%l2 = *(uchar *) (x!%i0 + 2)
ldub [%i0+2],%l2
# live registers: 7 int64, 0 double

# input line 46:   y2 = *(uchar *) (y + 2)
# y2!%l5 = *(uchar *) (y!%i1 + 2)
ldub [%i1+2],%l5
# live registers: 8 int64, 0 double

# input line 47:   d = y0 ^ x0
# d#2!%l0 = y0!%l3 ^ x0!%l0
xor %l3,%l0,%l0
# live registers: 7 int64, 0 double

# input line 48:   x3 = *(uchar *) (x + 3)
# x3!%l3 = *(uchar *) (x!%i0 + 3)
ldub [%i0+3],%l3
# live registers: 8 int64, 0 double

# input line 49:   y1 ^= x1
# y1#2!%l1 = y1!%l4 ^ x1!%l1
xor %l4,%l1,%l1
# live registers: 7 int64, 0 double

# input line 50:   y3 = *(uchar *) (y + 3)
# y3!%l4 = *(uchar *) (y!%i1 + 3)
ldub [%i1+3],%l4
# live registers: 8 int64, 0 double

# input line 51:   d |= y1
# d#3!%l0 = d#2!%l0 | y1#2!%l1
or %l0,%l1,%l0
# live registers: 7 int64, 0 double

# input line 52:   x4 = *(uchar *) (x + 4)
# x4!%l1 = *(uchar *) (x!%i0 + 4)
ldub [%i0+4],%l1
# live registers: 8 int64, 0 double

# input line 53:   y2 ^= x2
# y2#2!%l2 = y2!%l5 ^ x2!%l2
xor %l5,%l2,%l2
# live registers: 7 int64, 0 double

# input line 54:   y4 = *(uchar *) (y + 4)
# y4!%l5 = *(uchar *) (y!%i1 + 4)
ldub [%i1+4],%l5
# live registers: 8 int64, 0 double

# input line 55:   d |= y2
# d#4!%l0 = d#3!%l0 | y2#2!%l2
or %l0,%l2,%l0
# live registers: 7 int64, 0 double

# input line 56:   x5 = *(uchar *) (x + 5)
# x5!%l2 = *(uchar *) (x!%i0 + 5)
ldub [%i0+5],%l2
# live registers: 8 int64, 0 double

# input line 57:   y3 ^= x3
# y3#2!%l3 = y3!%l4 ^ x3!%l3
xor %l4,%l3,%l3
# live registers: 7 int64, 0 double

# input line 58:   y5 = *(uchar *) (y + 5)
# y5!%l4 = *(uchar *) (y!%i1 + 5)
ldub [%i1+5],%l4
# live registers: 8 int64, 0 double

# input line 59:   d |= y3
# d#5!%l0 = d#4!%l0 | y3#2!%l3
or %l0,%l3,%l0
# live registers: 7 int64, 0 double

# input line 60:   x6 = *(uchar *) (x + 6)
# x6!%l3 = *(uchar *) (x!%i0 + 6)
ldub [%i0+6],%l3
# live registers: 8 int64, 0 double

# input line 61:   y4 ^= x4
# y4#2!%l1 = y4!%l5 ^ x4!%l1
xor %l5,%l1,%l1
# live registers: 7 int64, 0 double

# input line 62:   y6 = *(uchar *) (y + 6)
# y6!%l5 = *(uchar *) (y!%i1 + 6)
ldub [%i1+6],%l5
# live registers: 8 int64, 0 double

# input line 63:   d |= y4
# d#6!%l0 = d#5!%l0 | y4#2!%l1
or %l0,%l1,%l0
# live registers: 7 int64, 0 double

# input line 64:   x7 = *(uchar *) (x + 7)
# x7!%l1 = *(uchar *) (x!%i0 + 7)
ldub [%i0+7],%l1
# live registers: 8 int64, 0 double

# input line 65:   y5 ^= x5
# y5#2!%l2 = y5!%l4 ^ x5!%l2
xor %l4,%l2,%l2
# live registers: 7 int64, 0 double

# input line 66:   y7 = *(uchar *) (y + 7)
# y7!%l4 = *(uchar *) (y!%i1 + 7)
ldub [%i1+7],%l4
# live registers: 8 int64, 0 double

# input line 67:   d |= y5
# d#7!%l0 = d#6!%l0 | y5#2!%l2
or %l0,%l2,%l0
# live registers: 7 int64, 0 double

# input line 68:   x8 = *(uchar *) (x + 8)
# x8!%l2 = *(uchar *) (x!%i0 + 8)
ldub [%i0+8],%l2
# live registers: 8 int64, 0 double

# input line 69:   y6 ^= x6
# y6#2!%l3 = y6!%l5 ^ x6!%l3
xor %l5,%l3,%l3
# live registers: 7 int64, 0 double

# input line 70:   y8 = *(uchar *) (y + 8)
# y8!%l5 = *(uchar *) (y!%i1 + 8)
ldub [%i1+8],%l5
# live registers: 8 int64, 0 double

# input line 71:   d |= y6
# d#8!%l0 = d#7!%l0 | y6#2!%l3
or %l0,%l3,%l0
# live registers: 7 int64, 0 double

# input line 72:   x9 = *(uchar *) (x + 9)
# x9!%l3 = *(uchar *) (x!%i0 + 9)
ldub [%i0+9],%l3
# live registers: 8 int64, 0 double

# input line 73:   y7 ^= x7
# y7#2!%l1 = y7!%l4 ^ x7!%l1
xor %l4,%l1,%l1
# live registers: 7 int64, 0 double

# input line 74:   y9 = *(uchar *) (y + 9)
# y9!%l4 = *(uchar *) (y!%i1 + 9)
ldub [%i1+9],%l4
# live registers: 8 int64, 0 double

# input line 75:   d |= y7
# d#9!%l0 = d#8!%l0 | y7#2!%l1
or %l0,%l1,%l0
# live registers: 7 int64, 0 double

# input line 76:   x10 = *(uchar *) (x + 10)
# x10!%l1 = *(uchar *) (x!%i0 + 10)
ldub [%i0+10],%l1
# live registers: 8 int64, 0 double

# input line 77:   y8 ^= x8
# y8#2!%l2 = y8!%l5 ^ x8!%l2
xor %l5,%l2,%l2
# live registers: 7 int64, 0 double

# input line 78:   y10 = *(uchar *) (y + 10)
# y10!%l5 = *(uchar *) (y!%i1 + 10)
ldub [%i1+10],%l5
# live registers: 8 int64, 0 double

# input line 79:   d |= y8
# d#10!%l0 = d#9!%l0 | y8#2!%l2
or %l0,%l2,%l0
# live registers: 7 int64, 0 double

# input line 80:   x11 = *(uchar *) (x + 11)
# x11!%l2 = *(uchar *) (x!%i0 + 11)
ldub [%i0+11],%l2
# live registers: 8 int64, 0 double

# input line 81:   y9 ^= x9
# y9#2!%l3 = y9!%l4 ^ x9!%l3
xor %l4,%l3,%l3
# live registers: 7 int64, 0 double

# input line 82:   y11 = *(uchar *) (y + 11)
# y11!%l4 = *(uchar *) (y!%i1 + 11)
ldub [%i1+11],%l4
# live registers: 8 int64, 0 double

# input line 83:   d |= y9
# d#11!%l0 = d#10!%l0 | y9#2!%l3
or %l0,%l3,%l0
# live registers: 7 int64, 0 double

# input line 84:   x12 = *(uchar *) (x + 12)
# x12!%l3 = *(uchar *) (x!%i0 + 12)
ldub [%i0+12],%l3
# live registers: 8 int64, 0 double

# input line 85:   y10 ^= x10
# y10#2!%l1 = y10!%l5 ^ x10!%l1
xor %l5,%l1,%l1
# live registers: 7 int64, 0 double

# input line 86:   y12 = *(uchar *) (y + 12)
# y12!%l5 = *(uchar *) (y!%i1 + 12)
ldub [%i1+12],%l5
# live registers: 8 int64, 0 double

# input line 87:   d |= y10
# d#12!%l0 = d#11!%l0 | y10#2!%l1
or %l0,%l1,%l0
# live registers: 7 int64, 0 double

# input line 88:   x13 = *(uchar *) (x + 13)
# x13!%l1 = *(uchar *) (x!%i0 + 13)
ldub [%i0+13],%l1
# live registers: 8 int64, 0 double

# input line 89:   y11 ^= x11
# y11#2!%l2 = y11!%l4 ^ x11!%l2
xor %l4,%l2,%l2
# live registers: 7 int64, 0 double

# input line 90:   y13 = *(uchar *) (y + 13)
# y13!%l4 = *(uchar *) (y!%i1 + 13)
ldub [%i1+13],%l4
# live registers: 8 int64, 0 double

# input line 91:   d |= y11
# d#13!%l0 = d#12!%l0 | y11#2!%l2
or %l0,%l2,%l0
# live registers: 7 int64, 0 double

# input line 92:   x14 = *(uchar *) (x + 14)
# x14!%l2 = *(uchar *) (x!%i0 + 14)
ldub [%i0+14],%l2
# live registers: 8 int64, 0 double

# input line 93:   y12 ^= x12
# y12#2!%l3 = y12!%l5 ^ x12!%l3
xor %l5,%l3,%l3
# live registers: 7 int64, 0 double

# input line 94:   y14 = *(uchar *) (y + 14)
# y14!%l5 = *(uchar *) (y!%i1 + 14)
ldub [%i1+14],%l5
# live registers: 8 int64, 0 double

# input line 95:   d |= y12
# d#14!%l0 = d#13!%l0 | y12#2!%l3
or %l0,%l3,%l0
# live registers: 7 int64, 0 double

# input line 96:   x15 = *(uchar *) (x + 15)
# x15!%l3 = *(uchar *) (x!%i0 + 15)
ldub [%i0+15],%l3
# live registers: 7 int64, 0 double

# input line 97:   y13 ^= x13
# y13#2!%l1 = y13!%l4 ^ x13!%l1
xor %l4,%l1,%l1
# live registers: 6 int64, 0 double

# input line 98:   y15 = *(uchar *) (y + 15)
# y15!%l4 = *(uchar *) (y!%i1 + 15)
ldub [%i1+15],%l4
# live registers: 6 int64, 0 double

# input line 99:   d |= y13
# d#15!%l0 = d#14!%l0 | y13#2!%l1
or %l0,%l1,%l0
# live registers: 5 int64, 0 double

# input line 100:   y14 ^= x14
# y14#2!%l1 = y14!%l5 ^ x14!%l2
xor %l5,%l2,%l1
# live registers: 4 int64, 0 double

# input line 101:   d |= y14
# d#16!%l0 = d#15!%l0 | y14#2!%l1
or %l0,%l1,%l0
# live registers: 3 int64, 0 double

# input line 102:   y15 ^= x15
# y15#2!%l1 = y15!%l4 ^ x15!%l3
xor %l4,%l3,%l1
# live registers: 2 int64, 0 double

# input line 103:   d |= y15
# d#17!%l0 = d#16!%l0 | y15#2!%l1
or %l0,%l1,%l0
# live registers: 1 int64, 0 double

# input line 104:   d -= 1
# d#18!%l0 = d#17!%l0 - 1
sub %l0,1,%l0
# live registers: 1 int64, 0 double

# input line 105:   (uint64) d >>= 8
# d!%i0 = (uint64) d#18!%l0 >> 8
srlx %l0,8,%i0
# live registers: 1 int64, 0 double

# input line 106: 

# input line 107: output d

# input line 108: leave
ret
restore
