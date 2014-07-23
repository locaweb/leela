# poly1305aes_aix_isequal.s version 20050205
# D. J. Bernstein
# Public domain.

# translated by qhasm-ppc version 20050205
.toc

# input line 1: register int32 d

# input line 2: register int32 result

# input line 3: register int32 x

# input line 4: register int32 y

# input line 5: register int32 x0

# input line 6: register int32 x1

# input line 7: register int32 x2

# input line 8: register int32 x3

# input line 9: register int32 y0

# input line 10: register int32 y1

# input line 11: register int32 y2

# input line 12: register int32 y3

# input line 13: 

# input line 14: enter poly1305aes_aix_isequal
.csect poly1305aes_aix_isequal[DS]
.globl poly1305aes_aix_isequal
poly1305aes_aix_isequal:
.long .poly1305aes_aix_isequal
.long TOC[tc0]
.long 0
.csect .text[PR]
.globl .poly1305aes_aix_isequal
.poly1305aes_aix_isequal:

# input line 15: input x

# input line 16: input y

# input line 17: 

# input line 18:   x0 = *(uint32 *) (x + 0)
# x0 = *(uint32 *) (x + 0)
# int32#3 = *(uint32 *) (int32#1 + 0)
# 5 = *(uint32 *) (3 + 0)
lwz 5,0(3)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 22
# live double values: 18
# live flags values: 0

# input line 19: 

# input line 20:   y0 = *(uint32 *) (y + 0)
# y0 = *(uint32 *) (y + 0)
# int32#6 = *(uint32 *) (int32#2 + 0)
# 8 = *(uint32 *) (4 + 0)
lwz 8,0(4)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 23
# live double values: 18
# live flags values: 0

# input line 21: 

# input line 22:   x1 = *(uint32 *) (x + 4)
# x1 = *(uint32 *) (x + 4)
# int32#4 = *(uint32 *) (int32#1 + 4)
# 6 = *(uint32 *) (3 + 4)
lwz 6,4(3)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 24
# live double values: 18
# live flags values: 0

# input line 23: 

# input line 24:   y1 = *(uint32 *) (y + 4)
# y1 = *(uint32 *) (y + 4)
# int32#7 = *(uint32 *) (int32#2 + 4)
# 9 = *(uint32 *) (4 + 4)
lwz 9,4(4)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 25
# live double values: 18
# live flags values: 0

# input line 25: 

# input line 26:   x2 = *(uint32 *) (x + 8)
# x2 = *(uint32 *) (x + 8)
# int32#5 = *(uint32 *) (int32#1 + 8)
# 7 = *(uint32 *) (3 + 8)
lwz 7,8(3)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 26
# live double values: 18
# live flags values: 0

# input line 27:   d = y0 ^ x0
# d = y0 ^ x0
# int32#3 = int32#6 ^ int32#3
# 5 = 8 ^ 5
xor 5,8,5
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 25
# live double values: 18
# live flags values: 0

# input line 28: 

# input line 29:   y2 = *(uint32 *) (y + 8)
# y2 = *(uint32 *) (y + 8)
# int32#8 = *(uint32 *) (int32#2 + 8)
# 10 = *(uint32 *) (4 + 8)
lwz 10,8(4)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 26
# live double values: 18
# live flags values: 0

# input line 30:   y1 ^= x1
# y1#2 = y1 ^ x1
# int32#6 = int32#7 ^ int32#4
# 8 = 9 ^ 6
xor 8,9,6
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 25
# live double values: 18
# live flags values: 0

# input line 31: 

# input line 32:   x3 = *(uint32 *) (x + 12)
# x3 = *(uint32 *) (x + 12)
# int32#4 = *(uint32 *) (int32#1 + 12)
# 6 = *(uint32 *) (3 + 12)
lwz 6,12(3)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 25
# live double values: 18
# live flags values: 0

# input line 33:   d |= y1
# d#2 = d | y1#2
# int32#1 = int32#3 | int32#6
# 3 = 5 | 8
or 3,5,8
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 24
# live double values: 18
# live flags values: 0

# input line 34: 

# input line 35:   y3 = *(uint32 *) (y + 12)
# y3 = *(uint32 *) (y + 12)
# int32#3 = *(uint32 *) (int32#2 + 12)
# 5 = *(uint32 *) (4 + 12)
lwz 5,12(4)
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 24
# live double values: 18
# live flags values: 0

# input line 36:   y2 ^= x2
# y2#2 = y2 ^ x2
# int32#2 = int32#8 ^ int32#5
# 4 = 10 ^ 7
xor 4,10,7
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 23
# live double values: 18
# live flags values: 0

# input line 37: 

# input line 38:   d |= y2
# d#3 = d#2 | y2#2
# int32#1 = int32#1 | int32#2
# 3 = 3 | 4
or 3,3,4
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 22
# live double values: 18
# live flags values: 0

# input line 39:   y3 ^= x3
# y3#2 = y3 ^ x3
# int32#2 = int32#3 ^ int32#4
# 4 = 5 ^ 6
xor 4,5,6
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 21
# live double values: 18
# live flags values: 0

# input line 40: 

# input line 41:   d |= y3
# d#4 = d#3 | y3#2
# int32#1 = int32#1 | int32#2
# 3 = 3 | 4
or 3,3,4
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 20
# live double values: 18
# live flags values: 0

# input line 42: 

# input line 43:   d -= 1
# d#5 = d#4 - 1
# int32#1 = int32#1 - 1
# 3 = 3 - 1
addi 3,3,-1
# live mem32 values: 0
# live flag values: 0
# live mem64 values: 0
# live int32 values: 20
# live double values: 18
# live flags values: 0

# input line 44:   

# input line 45:   carry d = d + 1
# carry d#6 = d#5 + 1
# carry int32#1 = int32#1 + 1
# carry 3 = 3 + 1
addic 3,3,1
# live mem32 values: 0
# live flag values: 1
# live mem64 values: 0
# live int32 values: 20
# live double values: 18
# live flags values: 0

# input line 46:   result = 0
# result#2 = 0
# int32#1 = 0
# 3 = 0
li 3,0
# live mem32 values: 0
# live flag values: 1
# live mem64 values: 0
# live int32 values: 20
# live double values: 18
# live flags values: 0

# input line 47: 

# input line 48:   kill d

# input line 49: 

# input line 50:   carry result = result + result + carry
# carry result = result#2 + result#2 + carry
# carry int32#1 = int32#1 + int32#1 + carry
# carry 3 = 3 + 3 + carry
adde 3,3,3
# live mem32 values: 0
# live flag values: 1
# live mem64 values: 0
# live int32 values: 20
# live double values: 18
# live flags values: 0

# input line 51: 

# input line 52: output result

# input line 53: leave
blr
