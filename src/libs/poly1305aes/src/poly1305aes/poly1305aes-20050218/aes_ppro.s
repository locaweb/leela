# aes_ppro.s version 20050213
# D. J. Bernstein
# Public domain.

# translated by qhasm-x86 version 20050213

## input line 1: temporary mem32 r4

## input line 2: temporary mem32 r5

## input line 3: temporary mem32 r6

## input line 4: temporary mem32 r7

## input line 5: temporary mem32 r8

## input line 6: temporary mem32 r9

## input line 7: temporary mem32 r10

## input line 8: temporary mem32 r11

## input line 9: temporary mem32 r12

## input line 10: temporary mem32 r13

## input line 11: temporary mem32 r14

## input line 12: temporary mem32 r15

## input line 13: temporary mem32 r16

## input line 14: temporary mem32 r17

## input line 15: temporary mem32 r18

## input line 16: temporary mem32 r19

## input line 17: temporary mem32 r20

## input line 18: temporary mem32 r21

## input line 19: temporary mem32 r22

## input line 20: temporary mem32 r23

## input line 21: temporary mem32 r24

## input line 22: temporary mem32 r25

## input line 23: temporary mem32 r26

## input line 24: temporary mem32 r27

## input line 25: temporary mem32 r28

## input line 26: temporary mem32 r29

## input line 27: temporary mem32 r30

## input line 28: temporary mem32 r31

## input line 29: temporary mem32 r32

## input line 30: temporary mem32 r33

## input line 31: temporary mem32 r34

## input line 32: temporary mem32 r35

## input line 33: temporary mem32 r36

## input line 34: temporary mem32 r37

## input line 35: temporary mem32 r38

## input line 36: temporary mem32 r39

## input line 37: temporary mem32 r40

## input line 38: temporary mem32 r41

## input line 39: temporary mem32 r42

## input line 40: temporary mem32 r43

## input line 41: register int32 b1

## input line 42: register int32 b3

## input line 43: register int32 b0

## input line 44: register int32 b2

## input line 45: register int32 a1

## input line 46: register int32 a3

## input line 47: register int32 a0

## input line 48: register int32 a2

## input line 49: register int32 q3

## input line 50: register int32 q2

## input line 51: register int32 q1

## input line 52: register int32 q0

## input line 53: register int32 out

## input line 54: register int32 k

## input line 55: register int32 n

## input line 56: register int32 e

## input line 57: register int32 p03

## input line 58: register int32 p13

## input line 59: register int32 p32

## input line 60: register int32 p23

## input line 61: register int32 p31

## input line 62: register int32 p33

## input line 63: register int32 p30

## input line 64: register int32 f

## input line 65: register int32 p00

## input line 66: register int32 p01

## input line 67: register int32 p02

## input line 68: register int32 p10

## input line 69: register int32 p11

## input line 70: register int32 p12

## input line 71: register int32 p20

## input line 72: register int32 p21

## input line 73: register int32 p22

## input line 74: 

## input line 75: stackstrategy 4096 &aes_ppro_constants

## input line 76: 

## input line 77: enter aes_ppro
.text
.p2align 5
.globl _aes_ppro
.globl aes_ppro
_aes_ppro:
aes_ppro:
mov %esp,%eax
sub $aes_ppro_constants,%eax
and $4095,%eax
add $208,%eax
sub %eax,%esp
## live mem32 values: 3
## live int32 values: 5
## live flags values: 0

## input line 78: input out

## input line 79: input k

## input line 80: input n

## input line 81: 

## input line 82:   store callerint ebx
movl %ebx,0(%esp)
## live mem32 values: 4
## live int32 values: 4
## live flags values: 0

## input line 83:   store callerint esi
movl %esi,4(%esp)
## live mem32 values: 5
## live int32 values: 3
## live flags values: 0

## input line 84:   store callerint edi
movl %edi,8(%esp)
## live mem32 values: 6
## live int32 values: 2
## live flags values: 0

## input line 85:   store callerint ebp
movl %ebp,12(%esp)
## live mem32 values: 7
## live int32 values: 1
## live flags values: 0

## input line 86: 

## input line 87:   load k
movl 8(%esp,%eax),%edx
## live mem32 values: 6
## live int32 values: 2
## live flags values: 0

## input line 88:   load n
movl 12(%esp,%eax),%edi
## live mem32 values: 5
## live int32 values: 3
## live flags values: 0

## input line 89: 

## input line 90:   store callerint stackoffset
movl %eax,16(%esp)
## live mem32 values: 6
## live int32 values: 2
## live flags values: 0

## input line 91: 

## input line 92:   a0 = *(uint32 *) (k + 0)
## a0 = *(uint32 *) (k + 0)
## int32#4 = *(uint32 *) (int32#3 + 0)
## %ebx = *(uint32 *) (%edx + 0)
movl 0(%edx),%ebx
## live mem32 values: 6
## live int32 values: 3
## live flags values: 0

## input line 93:   b0 = *(uint32 *) (n + 0)
## b0 = *(uint32 *) (n + 0)
## int32#1 = *(uint32 *) (int32#6 + 0)
## %eax = *(uint32 *) (%edi + 0)
movl 0(%edi),%eax
## live mem32 values: 6
## live int32 values: 4
## live flags values: 0

## input line 94:   inplace b0 ^= a0
## b0 ^= a0
## int32#1 ^= int32#4
## %eax ^= %ebx
xor %ebx,%eax
## live mem32 values: 6
## live int32 values: 4
## live flags values: 0

## input line 95:   store b0
movl %eax,188(%esp)
## live mem32 values: 7
## live int32 values: 3
## live flags values: 0

## input line 96: 

## input line 97:   a1 = *(uint32 *) (k + 4)
## a1 = *(uint32 *) (k + 4)
## int32#2 = *(uint32 *) (int32#3 + 4)
## %ecx = *(uint32 *) (%edx + 4)
movl 4(%edx),%ecx
## live mem32 values: 7
## live int32 values: 4
## live flags values: 0

## input line 98:   b1 = *(uint32 *) (n + 4)
## b1 = *(uint32 *) (n + 4)
## int32#1 = *(uint32 *) (int32#6 + 4)
## %eax = *(uint32 *) (%edi + 4)
movl 4(%edi),%eax
## live mem32 values: 7
## live int32 values: 5
## live flags values: 0

## input line 99:   inplace b1 ^= a1
## b1 ^= a1
## int32#1 ^= int32#2
## %eax ^= %ecx
xor %ecx,%eax
## live mem32 values: 7
## live int32 values: 5
## live flags values: 0

## input line 100:   store b1
movl %eax,180(%esp)
## live mem32 values: 8
## live int32 values: 4
## live flags values: 0

## input line 101: 

## input line 102:   a2 = *(uint32 *) (k + 8)
## a2 = *(uint32 *) (k + 8)
## int32#5 = *(uint32 *) (int32#3 + 8)
## %esi = *(uint32 *) (%edx + 8)
movl 8(%edx),%esi
## live mem32 values: 8
## live int32 values: 5
## live flags values: 0

## input line 103:   b2 = *(uint32 *) (n + 8)
## b2 = *(uint32 *) (n + 8)
## int32#1 = *(uint32 *) (int32#6 + 8)
## %eax = *(uint32 *) (%edi + 8)
movl 8(%edi),%eax
## live mem32 values: 8
## live int32 values: 6
## live flags values: 0

## input line 104:   inplace b2 ^= a2
## b2 ^= a2
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 8
## live int32 values: 6
## live flags values: 0

## input line 105:   inplace b2 <<<= 16
## b2 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 8
## live int32 values: 6
## live flags values: 0

## input line 106:   store b2
movl %eax,192(%esp)
## live mem32 values: 9
## live int32 values: 5
## live flags values: 0

## input line 107: 

## input line 108:   a3 = *(uint32 *) (k + 12)
## a3 = *(uint32 *) (k + 12)
## int32#3 = *(uint32 *) (int32#3 + 12)
## %edx = *(uint32 *) (%edx + 12)
movl 12(%edx),%edx
## live mem32 values: 9
## live int32 values: 5
## live flags values: 0

## input line 109:   b3 = *(uint32 *) (n + 12)
## b3 = *(uint32 *) (n + 12)
## int32#1 = *(uint32 *) (int32#6 + 12)
## %eax = *(uint32 *) (%edi + 12)
movl 12(%edi),%eax
## live mem32 values: 9
## live int32 values: 5
## live flags values: 0

## input line 110:   inplace b3 ^= a3
## b3 ^= a3
## int32#1 ^= int32#3
## %eax ^= %edx
xor %edx,%eax
## live mem32 values: 9
## live int32 values: 5
## live flags values: 0

## input line 111:   store b3
movl %eax,184(%esp)
## live mem32 values: 10
## live int32 values: 4
## live flags values: 0

## input line 112:   e = 0xff & a3
## e = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 113:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#2 = *(uint32 *) (8 * e + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 114:   inplace e &= 0xff000000
## e#2 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 115:   q1 = 0xff & (a3 >> 8)
## q1 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 116:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#2 = *(uint32 *) (8 * q1 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 117:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 118:   inplace a0 ^= 1
## a0 ^= 1
## int32#4 ^= 1
## %ebx ^= 1
xor $1,%ebx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 119:   inplace q1 &= 0xff
## q1#2 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 120:   inplace e ^= q1
## e#2 ^= q1#2
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 121:   q2 = 0xff & a3
## q2 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 122:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#2 = *(uint32 *) (8 * q2 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 123:   inplace q2 &= 0xff00
## q2#2 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 124:   q3 = 0xff & (a3 >> 8)
## q3 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 125:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#2 = *(uint32 *) (8 * q3 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 126:   inplace q3 &= 0xff0000
## q3#2 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 127:   inplace q3 ^= q2
## q3#2 ^= q2#2
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 128:   inplace e ^= q3
## e#2 ^= q3#2
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 129:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 130:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 131:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 132:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 133:   inplace a3 ^= e
## a3 ^= e#2
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 134:   inplace a2 ^= e
## a2 ^= e#2
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 135:   b2 = a2
## b2#2 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 136:   inplace b2 <<<= 16
## b2#2 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 137:   r6 = b2
movl %eax,28(%esp)
## live mem32 values: 11
## live int32 values: 5
## live flags values: 0

## input line 138:   r7 = a3
movl %edx,32(%esp)
## live mem32 values: 12
## live int32 values: 5
## live flags values: 0

## input line 139:   inplace a0 ^= e
## a0 ^= e#2
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 12
## live int32 values: 5
## live flags values: 0

## input line 140:   inplace a1 ^= e
## a1 ^= e#2
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 12
## live int32 values: 4
## live flags values: 0

## input line 141:   r4 = a0
movl %ebx,20(%esp)
## live mem32 values: 13
## live int32 values: 4
## live flags values: 0

## input line 142:   r5 = a1
movl %ecx,24(%esp)
## live mem32 values: 14
## live int32 values: 4
## live flags values: 0

## input line 143:   e = 0xff & a3
## e#3 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 144:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#4 = *(uint32 *) (8 * e#3 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 145:   inplace e &= 0xff000000
## e#4 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 146:   q1 = 0xff & (a3 >> 8)
## q1#3 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 147:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#4 = *(uint32 *) (8 * q1#3 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 148:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 149:   inplace a0 ^= 2
## a0 ^= 2
## int32#4 ^= 2
## %ebx ^= 2
xor $2,%ebx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 150:   inplace q1 &= 0xff
## q1#4 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 151:   inplace e ^= q1
## e#4 ^= q1#4
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 152:   q2 = 0xff & a3
## q2#3 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 153:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#4 = *(uint32 *) (8 * q2#3 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 154:   inplace q2 &= 0xff00
## q2#4 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 155:   q3 = 0xff & (a3 >> 8)
## q3#3 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 156:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#4 = *(uint32 *) (8 * q3#3 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 157:   inplace q3 &= 0xff0000
## q3#4 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 158:   inplace q3 ^= q2
## q3#4 ^= q2#4
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 159:   inplace e ^= q3
## e#4 ^= q3#4
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 160:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 161:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 162:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 163:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 164:   inplace a3 ^= e
## a3 ^= e#4
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 165:   inplace a2 ^= e
## a2 ^= e#4
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 166:   b2 = a2
## b2#3 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 167:   inplace b2 <<<= 16
## b2#3 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 168:   r10 = b2
movl %eax,44(%esp)
## live mem32 values: 15
## live int32 values: 5
## live flags values: 0

## input line 169:   r11 = a3
movl %edx,48(%esp)
## live mem32 values: 16
## live int32 values: 5
## live flags values: 0

## input line 170:   inplace a0 ^= e
## a0 ^= e#4
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 16
## live int32 values: 5
## live flags values: 0

## input line 171:   inplace a1 ^= e
## a1 ^= e#4
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 16
## live int32 values: 4
## live flags values: 0

## input line 172:   r8 = a0
movl %ebx,36(%esp)
## live mem32 values: 17
## live int32 values: 4
## live flags values: 0

## input line 173:   r9 = a1
movl %ecx,40(%esp)
## live mem32 values: 18
## live int32 values: 4
## live flags values: 0

## input line 174:   e = 0xff & a3
## e#5 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 175:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#6 = *(uint32 *) (8 * e#5 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 176:   inplace e &= 0xff000000
## e#6 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 177:   q1 = 0xff & (a3 >> 8)
## q1#5 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 178:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#6 = *(uint32 *) (8 * q1#5 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 179:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 180:   inplace a0 ^= 4
## a0 ^= 4
## int32#4 ^= 4
## %ebx ^= 4
xor $4,%ebx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 181:   inplace q1 &= 0xff
## q1#6 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 182:   inplace e ^= q1
## e#6 ^= q1#6
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 183:   q2 = 0xff & a3
## q2#5 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 184:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#6 = *(uint32 *) (8 * q2#5 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 185:   inplace q2 &= 0xff00
## q2#6 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 186:   q3 = 0xff & (a3 >> 8)
## q3#5 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 187:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#6 = *(uint32 *) (8 * q3#5 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 188:   inplace q3 &= 0xff0000
## q3#6 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 189:   inplace q3 ^= q2
## q3#6 ^= q2#6
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 190:   inplace e ^= q3
## e#6 ^= q3#6
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 191:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 192:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 193:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 194:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 195:   inplace a3 ^= e
## a3 ^= e#6
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 196:   inplace a2 ^= e
## a2 ^= e#6
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 197:   b2 = a2
## b2#4 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 198:   inplace b2 <<<= 16
## b2#4 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 199:   r14 = b2
movl %eax,60(%esp)
## live mem32 values: 19
## live int32 values: 5
## live flags values: 0

## input line 200:   r15 = a3
movl %edx,64(%esp)
## live mem32 values: 20
## live int32 values: 5
## live flags values: 0

## input line 201:   inplace a0 ^= e
## a0 ^= e#6
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 20
## live int32 values: 5
## live flags values: 0

## input line 202:   inplace a1 ^= e
## a1 ^= e#6
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 20
## live int32 values: 4
## live flags values: 0

## input line 203:   r12 = a0
movl %ebx,52(%esp)
## live mem32 values: 21
## live int32 values: 4
## live flags values: 0

## input line 204:   r13 = a1
movl %ecx,56(%esp)
## live mem32 values: 22
## live int32 values: 4
## live flags values: 0

## input line 205:   e = 0xff & a3
## e#7 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 206:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#8 = *(uint32 *) (8 * e#7 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 207:   inplace e &= 0xff000000
## e#8 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 208:   q1 = 0xff & (a3 >> 8)
## q1#7 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 209:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#8 = *(uint32 *) (8 * q1#7 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 210:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 211:   inplace a0 ^= 8
## a0 ^= 8
## int32#4 ^= 8
## %ebx ^= 8
xor $8,%ebx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 212:   inplace q1 &= 0xff
## q1#8 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 213:   inplace e ^= q1
## e#8 ^= q1#8
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 214:   q2 = 0xff & a3
## q2#7 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 215:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#8 = *(uint32 *) (8 * q2#7 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 216:   inplace q2 &= 0xff00
## q2#8 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 217:   q3 = 0xff & (a3 >> 8)
## q3#7 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 218:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#8 = *(uint32 *) (8 * q3#7 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 219:   inplace q3 &= 0xff0000
## q3#8 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 220:   inplace q3 ^= q2
## q3#8 ^= q2#8
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 221:   inplace e ^= q3
## e#8 ^= q3#8
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 222:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 223:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 224:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 225:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 226:   inplace a3 ^= e
## a3 ^= e#8
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 227:   inplace a2 ^= e
## a2 ^= e#8
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 228:   b2 = a2
## b2#5 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 229:   inplace b2 <<<= 16
## b2#5 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 230:   r18 = b2
movl %eax,76(%esp)
## live mem32 values: 23
## live int32 values: 5
## live flags values: 0

## input line 231:   r19 = a3
movl %edx,80(%esp)
## live mem32 values: 24
## live int32 values: 5
## live flags values: 0

## input line 232:   inplace a0 ^= e
## a0 ^= e#8
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 24
## live int32 values: 5
## live flags values: 0

## input line 233:   inplace a1 ^= e
## a1 ^= e#8
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 24
## live int32 values: 4
## live flags values: 0

## input line 234:   r16 = a0
movl %ebx,68(%esp)
## live mem32 values: 25
## live int32 values: 4
## live flags values: 0

## input line 235:   r17 = a1
movl %ecx,72(%esp)
## live mem32 values: 26
## live int32 values: 4
## live flags values: 0

## input line 236:   e = 0xff & a3
## e#9 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 237:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#10 = *(uint32 *) (8 * e#9 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 238:   inplace e &= 0xff000000
## e#10 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 239:   q1 = 0xff & (a3 >> 8)
## q1#9 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 240:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#10 = *(uint32 *) (8 * q1#9 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 241:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 242:   inplace a0 ^= 16
## a0 ^= 16
## int32#4 ^= 16
## %ebx ^= 16
xor $16,%ebx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 243:   inplace q1 &= 0xff
## q1#10 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 244:   inplace e ^= q1
## e#10 ^= q1#10
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 245:   q2 = 0xff & a3
## q2#9 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 246:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#10 = *(uint32 *) (8 * q2#9 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 247:   inplace q2 &= 0xff00
## q2#10 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 248:   q3 = 0xff & (a3 >> 8)
## q3#9 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 249:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#10 = *(uint32 *) (8 * q3#9 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 250:   inplace q3 &= 0xff0000
## q3#10 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 251:   inplace q3 ^= q2
## q3#10 ^= q2#10
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 252:   inplace e ^= q3
## e#10 ^= q3#10
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 253:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 254:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 255:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 256:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 257:   inplace a3 ^= e
## a3 ^= e#10
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 258:   inplace a2 ^= e
## a2 ^= e#10
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 259:   b2 = a2
## b2#6 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 260:   inplace b2 <<<= 16
## b2#6 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 261:   r22 = b2
movl %eax,92(%esp)
## live mem32 values: 27
## live int32 values: 5
## live flags values: 0

## input line 262:   r23 = a3
movl %edx,96(%esp)
## live mem32 values: 28
## live int32 values: 5
## live flags values: 0

## input line 263:   inplace a0 ^= e
## a0 ^= e#10
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 28
## live int32 values: 5
## live flags values: 0

## input line 264:   inplace a1 ^= e
## a1 ^= e#10
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 28
## live int32 values: 4
## live flags values: 0

## input line 265:   r20 = a0
movl %ebx,84(%esp)
## live mem32 values: 29
## live int32 values: 4
## live flags values: 0

## input line 266:   r21 = a1
movl %ecx,88(%esp)
## live mem32 values: 30
## live int32 values: 4
## live flags values: 0

## input line 267:   e = 0xff & a3
## e#11 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 268:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#12 = *(uint32 *) (8 * e#11 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 269:   inplace e &= 0xff000000
## e#12 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 270:   q1 = 0xff & (a3 >> 8)
## q1#11 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 271:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#12 = *(uint32 *) (8 * q1#11 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 272:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 273:   inplace a0 ^= 32
## a0 ^= 32
## int32#4 ^= 32
## %ebx ^= 32
xor $32,%ebx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 274:   inplace q1 &= 0xff
## q1#12 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 275:   inplace e ^= q1
## e#12 ^= q1#12
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 276:   q2 = 0xff & a3
## q2#11 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 277:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#12 = *(uint32 *) (8 * q2#11 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 278:   inplace q2 &= 0xff00
## q2#12 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 279:   q3 = 0xff & (a3 >> 8)
## q3#11 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 280:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#12 = *(uint32 *) (8 * q3#11 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 281:   inplace q3 &= 0xff0000
## q3#12 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 282:   inplace q3 ^= q2
## q3#12 ^= q2#12
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 283:   inplace e ^= q3
## e#12 ^= q3#12
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 284:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 285:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 286:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 287:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 288:   inplace a3 ^= e
## a3 ^= e#12
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 289:   inplace a2 ^= e
## a2 ^= e#12
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 290:   b2 = a2
## b2#7 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 291:   inplace b2 <<<= 16
## b2#7 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 292:   r26 = b2
movl %eax,108(%esp)
## live mem32 values: 31
## live int32 values: 5
## live flags values: 0

## input line 293:   r27 = a3
movl %edx,112(%esp)
## live mem32 values: 32
## live int32 values: 5
## live flags values: 0

## input line 294:   inplace a0 ^= e
## a0 ^= e#12
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 32
## live int32 values: 5
## live flags values: 0

## input line 295:   inplace a1 ^= e
## a1 ^= e#12
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 32
## live int32 values: 4
## live flags values: 0

## input line 296:   r24 = a0
movl %ebx,100(%esp)
## live mem32 values: 33
## live int32 values: 4
## live flags values: 0

## input line 297:   r25 = a1
movl %ecx,104(%esp)
## live mem32 values: 34
## live int32 values: 4
## live flags values: 0

## input line 298:   e = 0xff & a3
## e#13 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 299:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#14 = *(uint32 *) (8 * e#13 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 300:   inplace e &= 0xff000000
## e#14 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 301:   q1 = 0xff & (a3 >> 8)
## q1#13 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 302:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#14 = *(uint32 *) (8 * q1#13 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 303:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 304:   inplace a0 ^= 64
## a0 ^= 64
## int32#4 ^= 64
## %ebx ^= 64
xor $64,%ebx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 305:   inplace q1 &= 0xff
## q1#14 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 306:   inplace e ^= q1
## e#14 ^= q1#14
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 307:   q2 = 0xff & a3
## q2#13 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 308:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#14 = *(uint32 *) (8 * q2#13 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 309:   inplace q2 &= 0xff00
## q2#14 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 310:   q3 = 0xff & (a3 >> 8)
## q3#13 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 311:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#14 = *(uint32 *) (8 * q3#13 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 312:   inplace q3 &= 0xff0000
## q3#14 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 313:   inplace q3 ^= q2
## q3#14 ^= q2#14
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 314:   inplace e ^= q3
## e#14 ^= q3#14
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 315:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 316:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 317:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 318:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 319:   inplace a3 ^= e
## a3 ^= e#14
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 320:   inplace a2 ^= e
## a2 ^= e#14
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 321:   b2 = a2
## b2#8 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 322:   inplace b2 <<<= 16
## b2#8 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 323:   r30 = b2
movl %eax,124(%esp)
## live mem32 values: 35
## live int32 values: 5
## live flags values: 0

## input line 324:   r31 = a3
movl %edx,128(%esp)
## live mem32 values: 36
## live int32 values: 5
## live flags values: 0

## input line 325:   inplace a0 ^= e
## a0 ^= e#14
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 36
## live int32 values: 5
## live flags values: 0

## input line 326:   inplace a1 ^= e
## a1 ^= e#14
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 36
## live int32 values: 4
## live flags values: 0

## input line 327:   r28 = a0
movl %ebx,116(%esp)
## live mem32 values: 37
## live int32 values: 4
## live flags values: 0

## input line 328:   r29 = a1
movl %ecx,120(%esp)
## live mem32 values: 38
## live int32 values: 4
## live flags values: 0

## input line 329:   e = 0xff & a3
## e#15 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 330:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#16 = *(uint32 *) (8 * e#15 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 331:   inplace e &= 0xff000000
## e#16 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 332:   q1 = 0xff & (a3 >> 8)
## q1#15 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 333:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#16 = *(uint32 *) (8 * q1#15 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 334:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 335:   inplace a0 ^= 128
## a0 ^= 128
## int32#4 ^= 128
## %ebx ^= 128
xor $128,%ebx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 336:   inplace q1 &= 0xff
## q1#16 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 337:   inplace e ^= q1
## e#16 ^= q1#16
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 338:   q2 = 0xff & a3
## q2#15 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 339:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#16 = *(uint32 *) (8 * q2#15 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 340:   inplace q2 &= 0xff00
## q2#16 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 341:   q3 = 0xff & (a3 >> 8)
## q3#15 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 342:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#16 = *(uint32 *) (8 * q3#15 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 343:   inplace q3 &= 0xff0000
## q3#16 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 344:   inplace q3 ^= q2
## q3#16 ^= q2#16
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 345:   inplace e ^= q3
## e#16 ^= q3#16
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 346:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 347:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 348:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 349:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 350:   inplace a3 ^= e
## a3 ^= e#16
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 351:   inplace a2 ^= e
## a2 ^= e#16
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 352:   b2 = a2
## b2#9 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 353:   inplace b2 <<<= 16
## b2#9 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 354:   r34 = b2
movl %eax,140(%esp)
## live mem32 values: 39
## live int32 values: 5
## live flags values: 0

## input line 355:   r35 = a3
movl %edx,144(%esp)
## live mem32 values: 40
## live int32 values: 5
## live flags values: 0

## input line 356:   inplace a0 ^= e
## a0 ^= e#16
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 40
## live int32 values: 5
## live flags values: 0

## input line 357:   inplace a1 ^= e
## a1 ^= e#16
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 40
## live int32 values: 4
## live flags values: 0

## input line 358:   r32 = a0
movl %ebx,132(%esp)
## live mem32 values: 41
## live int32 values: 4
## live flags values: 0

## input line 359:   r33 = a1
movl %ecx,136(%esp)
## live mem32 values: 42
## live int32 values: 4
## live flags values: 0

## input line 360:   e = 0xff & a3
## e#17 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 361:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#18 = *(uint32 *) (8 * e#17 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 362:   inplace e &= 0xff000000
## e#18 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 363:   q1 = 0xff & (a3 >> 8)
## q1#17 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 364:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#18 = *(uint32 *) (8 * q1#17 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 365:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 366:   inplace a0 ^= 27
## a0 ^= 27
## int32#4 ^= 27
## %ebx ^= 27
xor $27,%ebx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 367:   inplace q1 &= 0xff
## q1#18 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 368:   inplace e ^= q1
## e#18 ^= q1#18
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 369:   q2 = 0xff & a3
## q2#17 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 370:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#18 = *(uint32 *) (8 * q2#17 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 371:   inplace q2 &= 0xff00
## q2#18 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 372:   q3 = 0xff & (a3 >> 8)
## q3#17 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 373:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#18 = *(uint32 *) (8 * q3#17 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 374:   inplace q3 &= 0xff0000
## q3#18 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 375:   inplace q3 ^= q2
## q3#18 ^= q2#18
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 376:   inplace e ^= q3
## e#18 ^= q3#18
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 377:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 378:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 379:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 380:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 381:   inplace a3 ^= e
## a3 ^= e#18
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 382:   inplace a2 ^= e
## a2 ^= e#18
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 383:   b2 = a2
## b2#10 = a2
## int32#1 = int32#5
## %eax = %esi
mov %esi,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 384:   inplace b2 <<<= 16
## b2#10 <<<= 16
## int32#1 <<<= 16
## %eax <<<= 16
rol $16,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 385:   r38 = b2
movl %eax,156(%esp)
## live mem32 values: 43
## live int32 values: 5
## live flags values: 0

## input line 386:   r39 = a3
movl %edx,160(%esp)
## live mem32 values: 44
## live int32 values: 5
## live flags values: 0

## input line 387:   inplace a0 ^= e
## a0 ^= e#18
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 44
## live int32 values: 5
## live flags values: 0

## input line 388:   inplace a1 ^= e
## a1 ^= e#18
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 44
## live int32 values: 4
## live flags values: 0

## input line 389:   r36 = a0
movl %ebx,148(%esp)
## live mem32 values: 45
## live int32 values: 4
## live flags values: 0

## input line 390:   r37 = a1
movl %ecx,152(%esp)
## live mem32 values: 46
## live int32 values: 4
## live flags values: 0

## input line 391:   e = 0xff & a3
## e#19 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 392:   e = *(uint32 *) (&aes_ppro_table2 + 8 * e)
## e#20 = *(uint32 *) (8 * e#19 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %eax + &aes_ppro_table2)
movl aes_ppro_table2(,%eax,8),%ebp
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 393:   inplace e &= 0xff000000
## e#20 &= 0xff000000
## int32#7 &= 0xff000000
## %ebp &= 0xff000000
and $0xff000000,%ebp
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 394:   q1 = 0xff & (a3 >> 8)
## q1#19 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 395:   q1 = *(uint32 *) (&aes_ppro_table3 + 8 * q1)
## q1#20 = *(uint32 *) (8 * q1#19 + &aes_ppro_table3)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table3)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table3)
movl aes_ppro_table3(,%eax,8),%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 396:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 397:   inplace a0 ^= 54
## a0 ^= 54
## int32#4 ^= 54
## %ebx ^= 54
xor $54,%ebx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 398:   inplace q1 &= 0xff
## q1#20 &= 0xff
## int32#1 &= 0xff
## %eax &= 0xff
and $0xff,%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 399:   inplace e ^= q1
## e#20 ^= q1#20
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 400:   q2 = 0xff & a3
## q2#19 = 0xff & a3
## int32#1 = 0xff & int32#3
## %eax = 0xff & %edx
movzbl %dl,%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 401:   q2 = *(uint32 *) (&aes_ppro_table0 + 8 * q2)
## q2#20 = *(uint32 *) (8 * q2#19 + &aes_ppro_table0)
## int32#6 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %edi = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%edi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 402:   inplace q2 &= 0xff00
## q2#20 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 403:   q3 = 0xff & (a3 >> 8)
## q3#19 = 0xff & (a3 >> 8)
## int32#1 = 0xff & (int32#3 >> 8)
## %eax = 0xff & (%edx >> 8)
movzbl %dh,%eax
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 404:   q3 = *(uint32 *) (&aes_ppro_table1 + 8 * q3)
## q3#20 = *(uint32 *) (8 * q3#19 + &aes_ppro_table1)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table1)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table1)
movl aes_ppro_table1(,%eax,8),%eax
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 405:   inplace q3 &= 0xff0000
## q3#20 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 406:   inplace q3 ^= q2
## q3#20 ^= q2#20
## int32#1 ^= int32#6
## %eax ^= %edi
xor %edi,%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 407:   inplace e ^= q3
## e#20 ^= q3#20
## int32#7 ^= int32#1
## %ebp ^= %eax
xor %eax,%ebp
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 408:   inplace a3 <<<= 16
## a3 <<<= 16
## int32#3 <<<= 16
## %edx <<<= 16
rol $16,%edx
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 409:   inplace a1 ^= a0
## a1 ^= a0
## int32#2 ^= int32#4
## %ecx ^= %ebx
xor %ebx,%ecx
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 410:   inplace a2 ^= a1
## a2 ^= a1
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 411:   inplace a3 ^= a2
## a3 ^= a2
## int32#3 ^= int32#5
## %edx ^= %esi
xor %esi,%edx
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 412:   inplace a3 ^= e
## a3 ^= e#20
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 413:   inplace a2 ^= e
## a2 ^= e#20
## int32#5 ^= int32#7
## %esi ^= %ebp
xor %ebp,%esi
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 414:   r42 = a2
movl %esi,172(%esp)
## live mem32 values: 47
## live int32 values: 4
## live flags values: 0

## input line 415:   r43 = a3
movl %edx,176(%esp)
## live mem32 values: 48
## live int32 values: 3
## live flags values: 0

## input line 416:   inplace a0 ^= e
## a0 ^= e#20
## int32#4 ^= int32#7
## %ebx ^= %ebp
xor %ebp,%ebx
## live mem32 values: 48
## live int32 values: 3
## live flags values: 0

## input line 417:   inplace a1 ^= e
## a1 ^= e#20
## int32#2 ^= int32#7
## %ecx ^= %ebp
xor %ebp,%ecx
## live mem32 values: 48
## live int32 values: 2
## live flags values: 0

## input line 418:   r40 = a0
movl %ebx,164(%esp)
## live mem32 values: 49
## live int32 values: 1
## live flags values: 0

## input line 419:   r41 = a1
movl %ecx,168(%esp)
## live mem32 values: 50
## live int32 values: 0
## live flags values: 0

## input line 420:   load b0
movl 188(%esp),%edx
## live mem32 values: 49
## live int32 values: 1
## live flags values: 0

## input line 421:   load b1
movl 180(%esp),%eax
## live mem32 values: 48
## live int32 values: 2
## live flags values: 0

## input line 422:   load b2
movl 192(%esp),%ebx
## live mem32 values: 47
## live int32 values: 3
## live flags values: 0

## input line 423:   load b3
movl 184(%esp),%ecx
## live mem32 values: 46
## live int32 values: 4
## live flags values: 0

## input line 424:   p00 = 0xff & b0
## p00 = 0xff & b0#2
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 425:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#2 = *(uint32 *) (8 * p00 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 46
## live int32 values: 5
## live flags values: 0

## input line 426:   p01 = 0xff & (b0 >> 8)
## p01 = 0xff & (b0#2 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 427:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#2 = *(uint32 *) (8 * p01 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 428:   p20 = 0xff & b2
## p20 = 0xff & b2#11
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 429:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#2 ^= *(uint32 *) (8 * p20 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 430:   p21 = 0xff & (b2 >> 8)
## p21 = 0xff & (b2#11 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 431:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#2 ^= *(uint32 *) (8 * p21 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 432:   p11 = 0xff & (b1 >> 8)
## p11 = 0xff & (b1#2 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 433:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#2 ^= *(uint32 *) (8 * p11 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 434:   p10 = 0xff & b1
## p10 = 0xff & b1#2
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 435:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#2 = *(uint32 *) (8 * p10 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 436:   inplace b1 &= 0xffff0000
## b1#2 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 437:   inplace unsigned b0 >>= 16
## unsigned b0#2 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 438:   inplace b1 |= b0
## b1#2 |= b0#2
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 439:   p30 = 0xff & b3
## p30 = 0xff & b3#2
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 440:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#2 ^= *(uint32 *) (8 * p30 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 441:   p03 = 0xff & (b1 >> 8)
## p03 = 0xff & (b1#2 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 442:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#2 ^= *(uint32 *) (8 * p03 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 443:   inplace unsigned b2 >>= 16
## unsigned b2#11 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 444:   p23 = 0xff & (b2 >> 8)
## p23 = 0xff & (b2#11 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 445:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#2 ^= *(uint32 *) (8 * p23 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 446:   p22 = 0xff & b2
## p22 = 0xff & b2#11
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 447:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#12 = *(uint32 *) (8 * p22 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 448:   p02 = 0xff & b1
## p02 = 0xff & b1#2
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 449:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#12 ^= *(uint32 *) (8 * p02 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 450:   p31 = 0xff & (b3 >> 8)
## p31 = 0xff & (b3#2 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 451:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#12 ^= *(uint32 *) (8 * p31 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 452:   inplace unsigned b3 >>= 16
## unsigned b3#2 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 453:   inplace unsigned b1 >>= 16
## unsigned b1#2 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 454:   p33 = 0xff & (b3 >> 8)
## p33 = 0xff & (b3#2 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 455:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#2 ^= *(uint32 *) (8 * p33 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 456:   p13 = 0xff & (b1 >> 8)
## p13 = 0xff & (b1#2 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 46
## live int32 values: 7
## live flags values: 0

## input line 457:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#12 ^= *(uint32 *) (8 * p13 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 46
## live int32 values: 6
## live flags values: 0

## input line 458:   b0 = r4
movl 20(%esp),%edx
## live mem32 values: 45
## live int32 values: 7
## live flags values: 0

## input line 459:   inplace b0 ^= a0
## b0#3 ^= a0#2
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 45
## live int32 values: 6
## live flags values: 0

## input line 460:   inplace b2 ^= r6
xorl 28(%esp),%ebx
## live mem32 values: 44
## live int32 values: 6
## live flags values: 0

## input line 461:   p32 = 0xff & b3
## p32 = 0xff & b3#2
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 44
## live int32 values: 6
## live flags values: 0

## input line 462:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#2 ^= *(uint32 *) (8 * p32 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 44
## live int32 values: 5
## live flags values: 0

## input line 463:   p12 = 0xff & b1
## p12 = 0xff & b1#2
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 44
## live int32 values: 5
## live flags values: 0

## input line 464:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#2 ^= *(uint32 *) (8 * p12 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 44
## live int32 values: 4
## live flags values: 0

## input line 465:   b1 = r5
movl 24(%esp),%eax
## live mem32 values: 43
## live int32 values: 5
## live flags values: 0

## input line 466:   inplace b1 ^= a1
## b1#3 ^= a1#2
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 43
## live int32 values: 4
## live flags values: 0

## input line 467:   b3 = r7
movl 32(%esp),%ecx
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 468:   inplace b3 ^= a3
## b3#3 ^= a3#2
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 42
## live int32 values: 4
## live flags values: 0

## input line 469:   p00 = 0xff & b0
## p00#2 = 0xff & b0#3
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 470:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#3 = *(uint32 *) (8 * p00#2 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 42
## live int32 values: 5
## live flags values: 0

## input line 471:   p01 = 0xff & (b0 >> 8)
## p01#2 = 0xff & (b0#3 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 472:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#3 = *(uint32 *) (8 * p01#2 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 473:   p20 = 0xff & b2
## p20#2 = 0xff & b2#12
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 474:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#3 ^= *(uint32 *) (8 * p20#2 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 475:   p21 = 0xff & (b2 >> 8)
## p21#2 = 0xff & (b2#12 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 476:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#3 ^= *(uint32 *) (8 * p21#2 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 477:   p11 = 0xff & (b1 >> 8)
## p11#2 = 0xff & (b1#3 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 478:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#3 ^= *(uint32 *) (8 * p11#2 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 479:   p10 = 0xff & b1
## p10#2 = 0xff & b1#3
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 480:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#3 = *(uint32 *) (8 * p10#2 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 481:   inplace b1 &= 0xffff0000
## b1#3 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 482:   inplace unsigned b0 >>= 16
## unsigned b0#3 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 483:   inplace b1 |= b0
## b1#3 |= b0#3
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 484:   p30 = 0xff & b3
## p30#2 = 0xff & b3#3
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 485:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#3 ^= *(uint32 *) (8 * p30#2 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 486:   p03 = 0xff & (b1 >> 8)
## p03#2 = 0xff & (b1#3 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 487:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#3 ^= *(uint32 *) (8 * p03#2 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 488:   inplace unsigned b2 >>= 16
## unsigned b2#12 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 489:   p23 = 0xff & (b2 >> 8)
## p23#2 = 0xff & (b2#12 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 490:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#3 ^= *(uint32 *) (8 * p23#2 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 491:   p22 = 0xff & b2
## p22#2 = 0xff & b2#12
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 492:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#13 = *(uint32 *) (8 * p22#2 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 493:   p02 = 0xff & b1
## p02#2 = 0xff & b1#3
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 494:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#13 ^= *(uint32 *) (8 * p02#2 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 495:   p31 = 0xff & (b3 >> 8)
## p31#2 = 0xff & (b3#3 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 496:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#13 ^= *(uint32 *) (8 * p31#2 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 497:   inplace unsigned b3 >>= 16
## unsigned b3#3 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 498:   inplace unsigned b1 >>= 16
## unsigned b1#3 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 499:   p33 = 0xff & (b3 >> 8)
## p33#2 = 0xff & (b3#3 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 500:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#3 ^= *(uint32 *) (8 * p33#2 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 501:   p13 = 0xff & (b1 >> 8)
## p13#2 = 0xff & (b1#3 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 42
## live int32 values: 7
## live flags values: 0

## input line 502:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#13 ^= *(uint32 *) (8 * p13#2 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 42
## live int32 values: 6
## live flags values: 0

## input line 503:   b0 = r8
movl 36(%esp),%edx
## live mem32 values: 41
## live int32 values: 7
## live flags values: 0

## input line 504:   inplace b0 ^= a0
## b0#4 ^= a0#3
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 41
## live int32 values: 6
## live flags values: 0

## input line 505:   inplace b2 ^= r10
xorl 44(%esp),%ebx
## live mem32 values: 40
## live int32 values: 6
## live flags values: 0

## input line 506:   p32 = 0xff & b3
## p32#2 = 0xff & b3#3
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 40
## live int32 values: 6
## live flags values: 0

## input line 507:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#3 ^= *(uint32 *) (8 * p32#2 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 40
## live int32 values: 5
## live flags values: 0

## input line 508:   p12 = 0xff & b1
## p12#2 = 0xff & b1#3
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 40
## live int32 values: 5
## live flags values: 0

## input line 509:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#3 ^= *(uint32 *) (8 * p12#2 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 40
## live int32 values: 4
## live flags values: 0

## input line 510:   b1 = r9
movl 40(%esp),%eax
## live mem32 values: 39
## live int32 values: 5
## live flags values: 0

## input line 511:   inplace b1 ^= a1
## b1#4 ^= a1#3
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 39
## live int32 values: 4
## live flags values: 0

## input line 512:   b3 = r11
movl 48(%esp),%ecx
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 513:   inplace b3 ^= a3
## b3#4 ^= a3#3
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 38
## live int32 values: 4
## live flags values: 0

## input line 514:   p00 = 0xff & b0
## p00#3 = 0xff & b0#4
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 515:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#4 = *(uint32 *) (8 * p00#3 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 38
## live int32 values: 5
## live flags values: 0

## input line 516:   p01 = 0xff & (b0 >> 8)
## p01#3 = 0xff & (b0#4 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 517:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#4 = *(uint32 *) (8 * p01#3 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 518:   p20 = 0xff & b2
## p20#3 = 0xff & b2#13
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 519:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#4 ^= *(uint32 *) (8 * p20#3 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 520:   p21 = 0xff & (b2 >> 8)
## p21#3 = 0xff & (b2#13 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 521:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#4 ^= *(uint32 *) (8 * p21#3 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 522:   p11 = 0xff & (b1 >> 8)
## p11#3 = 0xff & (b1#4 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 523:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#4 ^= *(uint32 *) (8 * p11#3 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 524:   p10 = 0xff & b1
## p10#3 = 0xff & b1#4
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 525:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#4 = *(uint32 *) (8 * p10#3 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 526:   inplace b1 &= 0xffff0000
## b1#4 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 527:   inplace unsigned b0 >>= 16
## unsigned b0#4 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 528:   inplace b1 |= b0
## b1#4 |= b0#4
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 529:   p30 = 0xff & b3
## p30#3 = 0xff & b3#4
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 530:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#4 ^= *(uint32 *) (8 * p30#3 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 531:   p03 = 0xff & (b1 >> 8)
## p03#3 = 0xff & (b1#4 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 532:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#4 ^= *(uint32 *) (8 * p03#3 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 533:   inplace unsigned b2 >>= 16
## unsigned b2#13 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 534:   p23 = 0xff & (b2 >> 8)
## p23#3 = 0xff & (b2#13 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 535:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#4 ^= *(uint32 *) (8 * p23#3 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 536:   p22 = 0xff & b2
## p22#3 = 0xff & b2#13
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 537:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#14 = *(uint32 *) (8 * p22#3 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 538:   p02 = 0xff & b1
## p02#3 = 0xff & b1#4
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 539:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#14 ^= *(uint32 *) (8 * p02#3 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 540:   p31 = 0xff & (b3 >> 8)
## p31#3 = 0xff & (b3#4 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 541:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#14 ^= *(uint32 *) (8 * p31#3 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 542:   inplace unsigned b3 >>= 16
## unsigned b3#4 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 543:   inplace unsigned b1 >>= 16
## unsigned b1#4 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 544:   p33 = 0xff & (b3 >> 8)
## p33#3 = 0xff & (b3#4 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 545:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#4 ^= *(uint32 *) (8 * p33#3 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 546:   p13 = 0xff & (b1 >> 8)
## p13#3 = 0xff & (b1#4 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 38
## live int32 values: 7
## live flags values: 0

## input line 547:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#14 ^= *(uint32 *) (8 * p13#3 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 38
## live int32 values: 6
## live flags values: 0

## input line 548:   b0 = r12
movl 52(%esp),%edx
## live mem32 values: 37
## live int32 values: 7
## live flags values: 0

## input line 549:   inplace b0 ^= a0
## b0#5 ^= a0#4
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 37
## live int32 values: 6
## live flags values: 0

## input line 550:   inplace b2 ^= r14
xorl 60(%esp),%ebx
## live mem32 values: 36
## live int32 values: 6
## live flags values: 0

## input line 551:   p32 = 0xff & b3
## p32#3 = 0xff & b3#4
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 36
## live int32 values: 6
## live flags values: 0

## input line 552:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#4 ^= *(uint32 *) (8 * p32#3 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 36
## live int32 values: 5
## live flags values: 0

## input line 553:   p12 = 0xff & b1
## p12#3 = 0xff & b1#4
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 36
## live int32 values: 5
## live flags values: 0

## input line 554:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#4 ^= *(uint32 *) (8 * p12#3 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 36
## live int32 values: 4
## live flags values: 0

## input line 555:   b1 = r13
movl 56(%esp),%eax
## live mem32 values: 35
## live int32 values: 5
## live flags values: 0

## input line 556:   inplace b1 ^= a1
## b1#5 ^= a1#4
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 35
## live int32 values: 4
## live flags values: 0

## input line 557:   b3 = r15
movl 64(%esp),%ecx
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 558:   inplace b3 ^= a3
## b3#5 ^= a3#4
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 34
## live int32 values: 4
## live flags values: 0

## input line 559:   p00 = 0xff & b0
## p00#4 = 0xff & b0#5
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 560:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#5 = *(uint32 *) (8 * p00#4 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 34
## live int32 values: 5
## live flags values: 0

## input line 561:   p01 = 0xff & (b0 >> 8)
## p01#4 = 0xff & (b0#5 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 562:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#5 = *(uint32 *) (8 * p01#4 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 563:   p20 = 0xff & b2
## p20#4 = 0xff & b2#14
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 564:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#5 ^= *(uint32 *) (8 * p20#4 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 565:   p21 = 0xff & (b2 >> 8)
## p21#4 = 0xff & (b2#14 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 566:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#5 ^= *(uint32 *) (8 * p21#4 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 567:   p11 = 0xff & (b1 >> 8)
## p11#4 = 0xff & (b1#5 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 568:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#5 ^= *(uint32 *) (8 * p11#4 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 569:   p10 = 0xff & b1
## p10#4 = 0xff & b1#5
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 570:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#5 = *(uint32 *) (8 * p10#4 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 571:   inplace b1 &= 0xffff0000
## b1#5 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 572:   inplace unsigned b0 >>= 16
## unsigned b0#5 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 573:   inplace b1 |= b0
## b1#5 |= b0#5
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 574:   p30 = 0xff & b3
## p30#4 = 0xff & b3#5
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 575:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#5 ^= *(uint32 *) (8 * p30#4 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 576:   p03 = 0xff & (b1 >> 8)
## p03#4 = 0xff & (b1#5 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 577:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#5 ^= *(uint32 *) (8 * p03#4 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 578:   inplace unsigned b2 >>= 16
## unsigned b2#14 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 579:   p23 = 0xff & (b2 >> 8)
## p23#4 = 0xff & (b2#14 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 580:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#5 ^= *(uint32 *) (8 * p23#4 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 581:   p22 = 0xff & b2
## p22#4 = 0xff & b2#14
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 582:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#15 = *(uint32 *) (8 * p22#4 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 583:   p02 = 0xff & b1
## p02#4 = 0xff & b1#5
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 584:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#15 ^= *(uint32 *) (8 * p02#4 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 585:   p31 = 0xff & (b3 >> 8)
## p31#4 = 0xff & (b3#5 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 586:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#15 ^= *(uint32 *) (8 * p31#4 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 587:   inplace unsigned b3 >>= 16
## unsigned b3#5 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 588:   inplace unsigned b1 >>= 16
## unsigned b1#5 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 589:   p33 = 0xff & (b3 >> 8)
## p33#4 = 0xff & (b3#5 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 590:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#5 ^= *(uint32 *) (8 * p33#4 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 591:   p13 = 0xff & (b1 >> 8)
## p13#4 = 0xff & (b1#5 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 34
## live int32 values: 7
## live flags values: 0

## input line 592:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#15 ^= *(uint32 *) (8 * p13#4 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 34
## live int32 values: 6
## live flags values: 0

## input line 593:   b0 = r16
movl 68(%esp),%edx
## live mem32 values: 33
## live int32 values: 7
## live flags values: 0

## input line 594:   inplace b0 ^= a0
## b0#6 ^= a0#5
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 33
## live int32 values: 6
## live flags values: 0

## input line 595:   inplace b2 ^= r18
xorl 76(%esp),%ebx
## live mem32 values: 32
## live int32 values: 6
## live flags values: 0

## input line 596:   p32 = 0xff & b3
## p32#4 = 0xff & b3#5
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 32
## live int32 values: 6
## live flags values: 0

## input line 597:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#5 ^= *(uint32 *) (8 * p32#4 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 32
## live int32 values: 5
## live flags values: 0

## input line 598:   p12 = 0xff & b1
## p12#4 = 0xff & b1#5
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 32
## live int32 values: 5
## live flags values: 0

## input line 599:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#5 ^= *(uint32 *) (8 * p12#4 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 32
## live int32 values: 4
## live flags values: 0

## input line 600:   b1 = r17
movl 72(%esp),%eax
## live mem32 values: 31
## live int32 values: 5
## live flags values: 0

## input line 601:   inplace b1 ^= a1
## b1#6 ^= a1#5
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 31
## live int32 values: 4
## live flags values: 0

## input line 602:   b3 = r19
movl 80(%esp),%ecx
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 603:   inplace b3 ^= a3
## b3#6 ^= a3#5
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 30
## live int32 values: 4
## live flags values: 0

## input line 604:   p00 = 0xff & b0
## p00#5 = 0xff & b0#6
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 605:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#6 = *(uint32 *) (8 * p00#5 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 30
## live int32 values: 5
## live flags values: 0

## input line 606:   p01 = 0xff & (b0 >> 8)
## p01#5 = 0xff & (b0#6 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 607:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#6 = *(uint32 *) (8 * p01#5 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 608:   p20 = 0xff & b2
## p20#5 = 0xff & b2#15
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 609:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#6 ^= *(uint32 *) (8 * p20#5 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 610:   p21 = 0xff & (b2 >> 8)
## p21#5 = 0xff & (b2#15 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 611:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#6 ^= *(uint32 *) (8 * p21#5 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 612:   p11 = 0xff & (b1 >> 8)
## p11#5 = 0xff & (b1#6 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 613:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#6 ^= *(uint32 *) (8 * p11#5 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 614:   p10 = 0xff & b1
## p10#5 = 0xff & b1#6
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 615:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#6 = *(uint32 *) (8 * p10#5 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 616:   inplace b1 &= 0xffff0000
## b1#6 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 617:   inplace unsigned b0 >>= 16
## unsigned b0#6 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 618:   inplace b1 |= b0
## b1#6 |= b0#6
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 619:   p30 = 0xff & b3
## p30#5 = 0xff & b3#6
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 620:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#6 ^= *(uint32 *) (8 * p30#5 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 621:   p03 = 0xff & (b1 >> 8)
## p03#5 = 0xff & (b1#6 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 622:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#6 ^= *(uint32 *) (8 * p03#5 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 623:   inplace unsigned b2 >>= 16
## unsigned b2#15 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 624:   p23 = 0xff & (b2 >> 8)
## p23#5 = 0xff & (b2#15 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 625:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#6 ^= *(uint32 *) (8 * p23#5 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 626:   p22 = 0xff & b2
## p22#5 = 0xff & b2#15
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 627:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#16 = *(uint32 *) (8 * p22#5 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 628:   p02 = 0xff & b1
## p02#5 = 0xff & b1#6
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 629:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#16 ^= *(uint32 *) (8 * p02#5 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 630:   p31 = 0xff & (b3 >> 8)
## p31#5 = 0xff & (b3#6 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 631:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#16 ^= *(uint32 *) (8 * p31#5 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 632:   inplace unsigned b3 >>= 16
## unsigned b3#6 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 633:   inplace unsigned b1 >>= 16
## unsigned b1#6 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 634:   p33 = 0xff & (b3 >> 8)
## p33#5 = 0xff & (b3#6 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 635:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#6 ^= *(uint32 *) (8 * p33#5 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 636:   p13 = 0xff & (b1 >> 8)
## p13#5 = 0xff & (b1#6 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 30
## live int32 values: 7
## live flags values: 0

## input line 637:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#16 ^= *(uint32 *) (8 * p13#5 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 30
## live int32 values: 6
## live flags values: 0

## input line 638:   b0 = r20
movl 84(%esp),%edx
## live mem32 values: 29
## live int32 values: 7
## live flags values: 0

## input line 639:   inplace b0 ^= a0
## b0#7 ^= a0#6
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 29
## live int32 values: 6
## live flags values: 0

## input line 640:   inplace b2 ^= r22
xorl 92(%esp),%ebx
## live mem32 values: 28
## live int32 values: 6
## live flags values: 0

## input line 641:   p32 = 0xff & b3
## p32#5 = 0xff & b3#6
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 28
## live int32 values: 6
## live flags values: 0

## input line 642:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#6 ^= *(uint32 *) (8 * p32#5 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 28
## live int32 values: 5
## live flags values: 0

## input line 643:   p12 = 0xff & b1
## p12#5 = 0xff & b1#6
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 28
## live int32 values: 5
## live flags values: 0

## input line 644:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#6 ^= *(uint32 *) (8 * p12#5 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 28
## live int32 values: 4
## live flags values: 0

## input line 645:   b1 = r21
movl 88(%esp),%eax
## live mem32 values: 27
## live int32 values: 5
## live flags values: 0

## input line 646:   inplace b1 ^= a1
## b1#7 ^= a1#6
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 27
## live int32 values: 4
## live flags values: 0

## input line 647:   b3 = r23
movl 96(%esp),%ecx
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 648:   inplace b3 ^= a3
## b3#7 ^= a3#6
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 26
## live int32 values: 4
## live flags values: 0

## input line 649:   p00 = 0xff & b0
## p00#6 = 0xff & b0#7
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 650:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#7 = *(uint32 *) (8 * p00#6 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 26
## live int32 values: 5
## live flags values: 0

## input line 651:   p01 = 0xff & (b0 >> 8)
## p01#6 = 0xff & (b0#7 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 652:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#7 = *(uint32 *) (8 * p01#6 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 653:   p20 = 0xff & b2
## p20#6 = 0xff & b2#16
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 654:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#7 ^= *(uint32 *) (8 * p20#6 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 655:   p21 = 0xff & (b2 >> 8)
## p21#6 = 0xff & (b2#16 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 656:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#7 ^= *(uint32 *) (8 * p21#6 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 657:   p11 = 0xff & (b1 >> 8)
## p11#6 = 0xff & (b1#7 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 658:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#7 ^= *(uint32 *) (8 * p11#6 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 659:   p10 = 0xff & b1
## p10#6 = 0xff & b1#7
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 660:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#7 = *(uint32 *) (8 * p10#6 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 661:   inplace b1 &= 0xffff0000
## b1#7 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 662:   inplace unsigned b0 >>= 16
## unsigned b0#7 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 663:   inplace b1 |= b0
## b1#7 |= b0#7
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 664:   p30 = 0xff & b3
## p30#6 = 0xff & b3#7
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 665:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#7 ^= *(uint32 *) (8 * p30#6 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 666:   p03 = 0xff & (b1 >> 8)
## p03#6 = 0xff & (b1#7 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 667:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#7 ^= *(uint32 *) (8 * p03#6 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 668:   inplace unsigned b2 >>= 16
## unsigned b2#16 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 669:   p23 = 0xff & (b2 >> 8)
## p23#6 = 0xff & (b2#16 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 670:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#7 ^= *(uint32 *) (8 * p23#6 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 671:   p22 = 0xff & b2
## p22#6 = 0xff & b2#16
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 672:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#17 = *(uint32 *) (8 * p22#6 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 673:   p02 = 0xff & b1
## p02#6 = 0xff & b1#7
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 674:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#17 ^= *(uint32 *) (8 * p02#6 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 675:   p31 = 0xff & (b3 >> 8)
## p31#6 = 0xff & (b3#7 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 676:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#17 ^= *(uint32 *) (8 * p31#6 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 677:   inplace unsigned b3 >>= 16
## unsigned b3#7 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 678:   inplace unsigned b1 >>= 16
## unsigned b1#7 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 679:   p33 = 0xff & (b3 >> 8)
## p33#6 = 0xff & (b3#7 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 680:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#7 ^= *(uint32 *) (8 * p33#6 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 681:   p13 = 0xff & (b1 >> 8)
## p13#6 = 0xff & (b1#7 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 26
## live int32 values: 7
## live flags values: 0

## input line 682:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#17 ^= *(uint32 *) (8 * p13#6 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 26
## live int32 values: 6
## live flags values: 0

## input line 683:   b0 = r24
movl 100(%esp),%edx
## live mem32 values: 25
## live int32 values: 7
## live flags values: 0

## input line 684:   inplace b0 ^= a0
## b0#8 ^= a0#7
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 25
## live int32 values: 6
## live flags values: 0

## input line 685:   inplace b2 ^= r26
xorl 108(%esp),%ebx
## live mem32 values: 24
## live int32 values: 6
## live flags values: 0

## input line 686:   p32 = 0xff & b3
## p32#6 = 0xff & b3#7
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 24
## live int32 values: 6
## live flags values: 0

## input line 687:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#7 ^= *(uint32 *) (8 * p32#6 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 24
## live int32 values: 5
## live flags values: 0

## input line 688:   p12 = 0xff & b1
## p12#6 = 0xff & b1#7
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 24
## live int32 values: 5
## live flags values: 0

## input line 689:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#7 ^= *(uint32 *) (8 * p12#6 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 24
## live int32 values: 4
## live flags values: 0

## input line 690:   b1 = r25
movl 104(%esp),%eax
## live mem32 values: 23
## live int32 values: 5
## live flags values: 0

## input line 691:   inplace b1 ^= a1
## b1#8 ^= a1#7
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 23
## live int32 values: 4
## live flags values: 0

## input line 692:   b3 = r27
movl 112(%esp),%ecx
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 693:   inplace b3 ^= a3
## b3#8 ^= a3#7
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 22
## live int32 values: 4
## live flags values: 0

## input line 694:   p00 = 0xff & b0
## p00#7 = 0xff & b0#8
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 695:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#8 = *(uint32 *) (8 * p00#7 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 22
## live int32 values: 5
## live flags values: 0

## input line 696:   p01 = 0xff & (b0 >> 8)
## p01#7 = 0xff & (b0#8 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 697:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#8 = *(uint32 *) (8 * p01#7 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 698:   p20 = 0xff & b2
## p20#7 = 0xff & b2#17
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 699:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#8 ^= *(uint32 *) (8 * p20#7 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 700:   p21 = 0xff & (b2 >> 8)
## p21#7 = 0xff & (b2#17 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 701:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#8 ^= *(uint32 *) (8 * p21#7 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 702:   p11 = 0xff & (b1 >> 8)
## p11#7 = 0xff & (b1#8 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 703:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#8 ^= *(uint32 *) (8 * p11#7 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 704:   p10 = 0xff & b1
## p10#7 = 0xff & b1#8
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 705:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#8 = *(uint32 *) (8 * p10#7 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 706:   inplace b1 &= 0xffff0000
## b1#8 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 707:   inplace unsigned b0 >>= 16
## unsigned b0#8 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 708:   inplace b1 |= b0
## b1#8 |= b0#8
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 709:   p30 = 0xff & b3
## p30#7 = 0xff & b3#8
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 710:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#8 ^= *(uint32 *) (8 * p30#7 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 711:   p03 = 0xff & (b1 >> 8)
## p03#7 = 0xff & (b1#8 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 712:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#8 ^= *(uint32 *) (8 * p03#7 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 713:   inplace unsigned b2 >>= 16
## unsigned b2#17 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 714:   p23 = 0xff & (b2 >> 8)
## p23#7 = 0xff & (b2#17 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 715:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#8 ^= *(uint32 *) (8 * p23#7 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 716:   p22 = 0xff & b2
## p22#7 = 0xff & b2#17
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 717:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#18 = *(uint32 *) (8 * p22#7 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 718:   p02 = 0xff & b1
## p02#7 = 0xff & b1#8
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 719:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#18 ^= *(uint32 *) (8 * p02#7 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 720:   p31 = 0xff & (b3 >> 8)
## p31#7 = 0xff & (b3#8 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 721:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#18 ^= *(uint32 *) (8 * p31#7 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 722:   inplace unsigned b3 >>= 16
## unsigned b3#8 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 723:   inplace unsigned b1 >>= 16
## unsigned b1#8 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 724:   p33 = 0xff & (b3 >> 8)
## p33#7 = 0xff & (b3#8 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 725:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#8 ^= *(uint32 *) (8 * p33#7 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 726:   p13 = 0xff & (b1 >> 8)
## p13#7 = 0xff & (b1#8 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 22
## live int32 values: 7
## live flags values: 0

## input line 727:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#18 ^= *(uint32 *) (8 * p13#7 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 22
## live int32 values: 6
## live flags values: 0

## input line 728:   b0 = r28
movl 116(%esp),%edx
## live mem32 values: 21
## live int32 values: 7
## live flags values: 0

## input line 729:   inplace b0 ^= a0
## b0#9 ^= a0#8
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 21
## live int32 values: 6
## live flags values: 0

## input line 730:   inplace b2 ^= r30
xorl 124(%esp),%ebx
## live mem32 values: 20
## live int32 values: 6
## live flags values: 0

## input line 731:   p32 = 0xff & b3
## p32#7 = 0xff & b3#8
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 20
## live int32 values: 6
## live flags values: 0

## input line 732:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#8 ^= *(uint32 *) (8 * p32#7 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 20
## live int32 values: 5
## live flags values: 0

## input line 733:   p12 = 0xff & b1
## p12#7 = 0xff & b1#8
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 20
## live int32 values: 5
## live flags values: 0

## input line 734:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#8 ^= *(uint32 *) (8 * p12#7 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 20
## live int32 values: 4
## live flags values: 0

## input line 735:   b1 = r29
movl 120(%esp),%eax
## live mem32 values: 19
## live int32 values: 5
## live flags values: 0

## input line 736:   inplace b1 ^= a1
## b1#9 ^= a1#8
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 19
## live int32 values: 4
## live flags values: 0

## input line 737:   b3 = r31
movl 128(%esp),%ecx
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 738:   inplace b3 ^= a3
## b3#9 ^= a3#8
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 18
## live int32 values: 4
## live flags values: 0

## input line 739:   p00 = 0xff & b0
## p00#8 = 0xff & b0#9
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 740:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#9 = *(uint32 *) (8 * p00#8 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 18
## live int32 values: 5
## live flags values: 0

## input line 741:   p01 = 0xff & (b0 >> 8)
## p01#8 = 0xff & (b0#9 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 742:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#9 = *(uint32 *) (8 * p01#8 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 743:   p20 = 0xff & b2
## p20#8 = 0xff & b2#18
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 744:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#9 ^= *(uint32 *) (8 * p20#8 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 745:   p21 = 0xff & (b2 >> 8)
## p21#8 = 0xff & (b2#18 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 746:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#9 ^= *(uint32 *) (8 * p21#8 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 747:   p11 = 0xff & (b1 >> 8)
## p11#8 = 0xff & (b1#9 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 748:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#9 ^= *(uint32 *) (8 * p11#8 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 749:   p10 = 0xff & b1
## p10#8 = 0xff & b1#9
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 750:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#9 = *(uint32 *) (8 * p10#8 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 751:   inplace b1 &= 0xffff0000
## b1#9 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 752:   inplace unsigned b0 >>= 16
## unsigned b0#9 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 753:   inplace b1 |= b0
## b1#9 |= b0#9
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 754:   p30 = 0xff & b3
## p30#8 = 0xff & b3#9
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 755:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#9 ^= *(uint32 *) (8 * p30#8 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 756:   p03 = 0xff & (b1 >> 8)
## p03#8 = 0xff & (b1#9 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 757:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#9 ^= *(uint32 *) (8 * p03#8 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 758:   inplace unsigned b2 >>= 16
## unsigned b2#18 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 759:   p23 = 0xff & (b2 >> 8)
## p23#8 = 0xff & (b2#18 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 760:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#9 ^= *(uint32 *) (8 * p23#8 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 761:   p22 = 0xff & b2
## p22#8 = 0xff & b2#18
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 762:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#19 = *(uint32 *) (8 * p22#8 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 763:   p02 = 0xff & b1
## p02#8 = 0xff & b1#9
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 764:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#19 ^= *(uint32 *) (8 * p02#8 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 765:   p31 = 0xff & (b3 >> 8)
## p31#8 = 0xff & (b3#9 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 766:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#19 ^= *(uint32 *) (8 * p31#8 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 767:   inplace unsigned b3 >>= 16
## unsigned b3#9 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 768:   inplace unsigned b1 >>= 16
## unsigned b1#9 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 769:   p33 = 0xff & (b3 >> 8)
## p33#8 = 0xff & (b3#9 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 770:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#9 ^= *(uint32 *) (8 * p33#8 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 771:   p13 = 0xff & (b1 >> 8)
## p13#8 = 0xff & (b1#9 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 18
## live int32 values: 7
## live flags values: 0

## input line 772:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#19 ^= *(uint32 *) (8 * p13#8 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 18
## live int32 values: 6
## live flags values: 0

## input line 773:   b0 = r32
movl 132(%esp),%edx
## live mem32 values: 17
## live int32 values: 7
## live flags values: 0

## input line 774:   inplace b0 ^= a0
## b0#10 ^= a0#9
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 17
## live int32 values: 6
## live flags values: 0

## input line 775:   inplace b2 ^= r34
xorl 140(%esp),%ebx
## live mem32 values: 16
## live int32 values: 6
## live flags values: 0

## input line 776:   p32 = 0xff & b3
## p32#8 = 0xff & b3#9
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 16
## live int32 values: 6
## live flags values: 0

## input line 777:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#9 ^= *(uint32 *) (8 * p32#8 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 16
## live int32 values: 5
## live flags values: 0

## input line 778:   p12 = 0xff & b1
## p12#8 = 0xff & b1#9
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 16
## live int32 values: 5
## live flags values: 0

## input line 779:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#9 ^= *(uint32 *) (8 * p12#8 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 16
## live int32 values: 4
## live flags values: 0

## input line 780:   b1 = r33
movl 136(%esp),%eax
## live mem32 values: 15
## live int32 values: 5
## live flags values: 0

## input line 781:   inplace b1 ^= a1
## b1#10 ^= a1#9
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 15
## live int32 values: 4
## live flags values: 0

## input line 782:   b3 = r35
movl 144(%esp),%ecx
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 783:   inplace b3 ^= a3
## b3#10 ^= a3#9
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 14
## live int32 values: 4
## live flags values: 0

## input line 784:   p00 = 0xff & b0
## p00#9 = 0xff & b0#10
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 785:   a0 = *(uint32 *) (&aes_ppro_table0 + 8 * p00)
## a0#10 = *(uint32 *) (8 * p00#9 + &aes_ppro_table0)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%ebp
## live mem32 values: 14
## live int32 values: 5
## live flags values: 0

## input line 786:   p01 = 0xff & (b0 >> 8)
## p01#9 = 0xff & (b0#10 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 787:   a3 = *(uint32 *) (&aes_ppro_table1 + 8 * p01)
## a3#10 = *(uint32 *) (8 * p01#9 + &aes_ppro_table1)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%edi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 788:   p20 = 0xff & b2
## p20#9 = 0xff & b2#19
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 789:   inplace a0 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p20)
## a0#10 ^= *(uint32 *) (8 * p20#9 + &aes_ppro_table2)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table2)
xorl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 790:   p21 = 0xff & (b2 >> 8)
## p21#9 = 0xff & (b2#19 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 791:   inplace a3 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p21)
## a3#10 ^= *(uint32 *) (8 * p21#9 + &aes_ppro_table3)
## int32#6 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi ^= *(uint32 *) (8 * %esi + &aes_ppro_table3)
xorl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 792:   p11 = 0xff & (b1 >> 8)
## p11#9 = 0xff & (b1#10 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 793:   inplace a0 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p11)
## a0#10 ^= *(uint32 *) (8 * p11#9 + &aes_ppro_table1)
## int32#7 ^= *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %ebp ^= *(uint32 *) (8 * %esi + &aes_ppro_table1)
xorl aes_ppro_table1(,%esi,8),%ebp
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 794:   p10 = 0xff & b1
## p10#9 = 0xff & b1#10
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 795:   a1 = *(uint32 *) (&aes_ppro_table0 + 8 * p10)
## a1#10 = *(uint32 *) (8 * p10#9 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 796:   inplace b1 &= 0xffff0000
## b1#10 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 797:   inplace unsigned b0 >>= 16
## unsigned b0#10 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 798:   inplace b1 |= b0
## b1#10 |= b0#10
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 799:   p30 = 0xff & b3
## p30#9 = 0xff & b3#10
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 800:   inplace a3 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p30)
## a3#10 ^= *(uint32 *) (8 * p30#9 + &aes_ppro_table0)
## int32#6 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %edi ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%edi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 801:   p03 = 0xff & (b1 >> 8)
## p03#9 = 0xff & (b1#10 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 802:   inplace a1 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p03)
## a1#10 ^= *(uint32 *) (8 * p03#9 + &aes_ppro_table3)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%esi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 803:   inplace unsigned b2 >>= 16
## unsigned b2#19 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 804:   p23 = 0xff & (b2 >> 8)
## p23#9 = 0xff & (b2#19 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 805:   inplace a1 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p23)
## a1#10 ^= *(uint32 *) (8 * p23#9 + &aes_ppro_table1)
## int32#5 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %esi ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%esi
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 806:   p22 = 0xff & b2
## p22#9 = 0xff & b2#19
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 807:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#20 = *(uint32 *) (8 * p22#9 + &aes_ppro_table2)
## int32#4 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %ebx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%ebx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 808:   p02 = 0xff & b1
## p02#9 = 0xff & b1#10
## int32#3 = 0xff & int32#1
## %edx = 0xff & %eax
movzbl %al,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 809:   inplace b2 ^= *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## b2#20 ^= *(uint32 *) (8 * p02#9 + &aes_ppro_table0)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table0)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table0)
xorl aes_ppro_table0(,%edx,8),%ebx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 810:   p31 = 0xff & (b3 >> 8)
## p31#9 = 0xff & (b3#10 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 811:   inplace b2 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## b2#20 ^= *(uint32 *) (8 * p31#9 + &aes_ppro_table3)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 812:   inplace unsigned b3 >>= 16
## unsigned b3#10 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 813:   inplace unsigned b1 >>= 16
## unsigned b1#10 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 814:   p33 = 0xff & (b3 >> 8)
## p33#9 = 0xff & (b3#10 >> 8)
## int32#3 = 0xff & (int32#2 >> 8)
## %edx = 0xff & (%ecx >> 8)
movzbl %ch,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 815:   inplace a0 ^= *(uint32 *) (&aes_ppro_table3 + 8 * p33)
## a0#10 ^= *(uint32 *) (8 * p33#9 + &aes_ppro_table3)
## int32#7 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %ebp ^= *(uint32 *) (8 * %edx + &aes_ppro_table3)
xorl aes_ppro_table3(,%edx,8),%ebp
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 816:   p13 = 0xff & (b1 >> 8)
## p13#9 = 0xff & (b1#10 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 14
## live int32 values: 7
## live flags values: 0

## input line 817:   inplace b2 ^= *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## b2#20 ^= *(uint32 *) (8 * p13#9 + &aes_ppro_table1)
## int32#4 ^= *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %ebx ^= *(uint32 *) (8 * %edx + &aes_ppro_table1)
xorl aes_ppro_table1(,%edx,8),%ebx
## live mem32 values: 14
## live int32 values: 6
## live flags values: 0

## input line 818:   b0 = r36
movl 148(%esp),%edx
## live mem32 values: 13
## live int32 values: 7
## live flags values: 0

## input line 819:   inplace b0 ^= a0
## b0#11 ^= a0#10
## int32#3 ^= int32#7
## %edx ^= %ebp
xor %ebp,%edx
## live mem32 values: 13
## live int32 values: 6
## live flags values: 0

## input line 820:   inplace b2 ^= r38
xorl 156(%esp),%ebx
## live mem32 values: 12
## live int32 values: 6
## live flags values: 0

## input line 821:   p32 = 0xff & b3
## p32#9 = 0xff & b3#10
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 12
## live int32 values: 6
## live flags values: 0

## input line 822:   inplace a1 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p32)
## a1#10 ^= *(uint32 *) (8 * p32#9 + &aes_ppro_table2)
## int32#5 ^= *(uint32 *) (8 * int32#2 + &aes_ppro_table2)
## %esi ^= *(uint32 *) (8 * %ecx + &aes_ppro_table2)
xorl aes_ppro_table2(,%ecx,8),%esi
## live mem32 values: 12
## live int32 values: 5
## live flags values: 0

## input line 823:   p12 = 0xff & b1
## p12#9 = 0xff & b1#10
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 12
## live int32 values: 5
## live flags values: 0

## input line 824:   inplace a3 ^= *(uint32 *) (&aes_ppro_table2 + 8 * p12)
## a3#10 ^= *(uint32 *) (8 * p12#9 + &aes_ppro_table2)
## int32#6 ^= *(uint32 *) (8 * int32#1 + &aes_ppro_table2)
## %edi ^= *(uint32 *) (8 * %eax + &aes_ppro_table2)
xorl aes_ppro_table2(,%eax,8),%edi
## live mem32 values: 12
## live int32 values: 4
## live flags values: 0

## input line 825:   b1 = r37
movl 152(%esp),%eax
## live mem32 values: 11
## live int32 values: 5
## live flags values: 0

## input line 826:   inplace b1 ^= a1
## b1#11 ^= a1#10
## int32#1 ^= int32#5
## %eax ^= %esi
xor %esi,%eax
## live mem32 values: 11
## live int32 values: 4
## live flags values: 0

## input line 827:   b3 = r39
movl 160(%esp),%ecx
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 828:   inplace b3 ^= a3
## b3#11 ^= a3#10
## int32#2 ^= int32#6
## %ecx ^= %edi
xor %edi,%ecx
## live mem32 values: 10
## live int32 values: 4
## live flags values: 0

## input line 829:   p00 = 0xff & b0
## p00#10 = 0xff & b0#11
## int32#5 = 0xff & int32#3
## %esi = 0xff & %edx
movzbl %dl,%esi
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 830:   a0 = *(uint32 *) (&aes_ppro_table2 + 8 * p00)
## a0#11 = *(uint32 *) (8 * p00#10 + &aes_ppro_table2)
## int32#7 = *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %ebp = *(uint32 *) (8 * %esi + &aes_ppro_table2)
movl aes_ppro_table2(,%esi,8),%ebp
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 831:   inplace a0 &= 0xff
## a0#11 &= 0xff
## int32#7 &= 0xff
## %ebp &= 0xff
and $0xff,%ebp
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 832:   p01 = 0xff & (b0 >> 8)
## p01#10 = 0xff & (b0#11 >> 8)
## int32#5 = 0xff & (int32#3 >> 8)
## %esi = 0xff & (%edx >> 8)
movzbl %dh,%esi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 833:   a3 = *(uint32 *) (&aes_ppro_table3 + 8 * p01)
## a3#11 = *(uint32 *) (8 * p01#10 + &aes_ppro_table3)
## int32#6 = *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %edi = *(uint32 *) (8 * %esi + &aes_ppro_table3)
movl aes_ppro_table3(,%esi,8),%edi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 834:   inplace a3 &= 0xff00
## a3#11 &= 0xff00
## int32#6 &= 0xff00
## %edi &= 0xff00
and $0xff00,%edi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 835:   p20 = 0xff & b2
## p20#10 = 0xff & b2#20
## int32#5 = 0xff & int32#4
## %esi = 0xff & %ebx
movzbl %bl,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 836:   p20 = *(uint32 *) (&aes_ppro_table0 + 8 * p20)
## p20#11 = *(uint32 *) (8 * p20#10 + &aes_ppro_table0)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table0)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table0)
movl aes_ppro_table0(,%esi,8),%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 837:   inplace p20 &= 0xff0000
## p20#11 &= 0xff0000
## int32#5 &= 0xff0000
## %esi &= 0xff0000
and $0xff0000,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 838:   inplace a0 ^= p20
## a0#11 ^= p20#11
## int32#7 ^= int32#5
## %ebp ^= %esi
xor %esi,%ebp
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 839:   p21 = 0xff & (b2 >> 8)
## p21#10 = 0xff & (b2#20 >> 8)
## int32#5 = 0xff & (int32#4 >> 8)
## %esi = 0xff & (%ebx >> 8)
movzbl %bh,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 840:   p21 = *(uint32 *) (&aes_ppro_table1 + 8 * p21)
## p21#11 = *(uint32 *) (8 * p21#10 + &aes_ppro_table1)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table1)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table1)
movl aes_ppro_table1(,%esi,8),%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 841:   inplace p21 &= 0xff000000
## p21#11 &= 0xff000000
## int32#5 &= 0xff000000
## %esi &= 0xff000000
and $0xff000000,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 842:   inplace a3 ^= p21
## a3#11 ^= p21#11
## int32#6 ^= int32#5
## %edi ^= %esi
xor %esi,%edi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 843:   p11 = 0xff & (b1 >> 8)
## p11#10 = 0xff & (b1#11 >> 8)
## int32#5 = 0xff & (int32#1 >> 8)
## %esi = 0xff & (%eax >> 8)
movzbl %ah,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 844:   p11 = *(uint32 *) (&aes_ppro_table3 + 8 * p11)
## p11#11 = *(uint32 *) (8 * p11#10 + &aes_ppro_table3)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table3)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table3)
movl aes_ppro_table3(,%esi,8),%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 845:   inplace p11 &= 0xff00
## p11#11 &= 0xff00
## int32#5 &= 0xff00
## %esi &= 0xff00
and $0xff00,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 846:   inplace a0 ^= p11
## a0#11 ^= p11#11
## int32#7 ^= int32#5
## %ebp ^= %esi
xor %esi,%ebp
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 847:   p10 = 0xff & b1
## p10#10 = 0xff & b1#11
## int32#5 = 0xff & int32#1
## %esi = 0xff & %eax
movzbl %al,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 848:   a1 = *(uint32 *) (&aes_ppro_table2 + 8 * p10)
## a1#11 = *(uint32 *) (8 * p10#10 + &aes_ppro_table2)
## int32#5 = *(uint32 *) (8 * int32#5 + &aes_ppro_table2)
## %esi = *(uint32 *) (8 * %esi + &aes_ppro_table2)
movl aes_ppro_table2(,%esi,8),%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 849:   inplace a1 &= 0xff
## a1#11 &= 0xff
## int32#5 &= 0xff
## %esi &= 0xff
and $0xff,%esi
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 850:   inplace b1 &= 0xffff0000
## b1#11 &= 0xffff0000
## int32#1 &= 0xffff0000
## %eax &= 0xffff0000
and $0xffff0000,%eax
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 851:   inplace unsigned b0 >>= 16
## unsigned b0#11 >>= 16
## unsigned int32#3 >>= 16
## unsigned %edx >>= 16
shr $16,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 852:   inplace b1 |= b0
## b1#11 |= b0#11
## int32#1 |= int32#3
## %eax |= %edx
or %edx,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 853:   p30 = 0xff & b3
## p30#10 = 0xff & b3#11
## int32#3 = 0xff & int32#2
## %edx = 0xff & %ecx
movzbl %cl,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 854:   p30 = *(uint32 *) (&aes_ppro_table2 + 8 * p30)
## p30#11 = *(uint32 *) (8 * p30#10 + &aes_ppro_table2)
## int32#3 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %edx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 855:   inplace p30 &= 0xff
## p30#11 &= 0xff
## int32#3 &= 0xff
## %edx &= 0xff
and $0xff,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 856:   inplace a3 ^= p30
## a3#11 ^= p30#11
## int32#6 ^= int32#3
## %edi ^= %edx
xor %edx,%edi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 857:   p03 = 0xff & (b1 >> 8)
## p03#10 = 0xff & (b1#11 >> 8)
## int32#3 = 0xff & (int32#1 >> 8)
## %edx = 0xff & (%eax >> 8)
movzbl %ah,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 858:   p03 = *(uint32 *) (&aes_ppro_table1 + 8 * p03)
## p03#11 = *(uint32 *) (8 * p03#10 + &aes_ppro_table1)
## int32#3 = *(uint32 *) (8 * int32#3 + &aes_ppro_table1)
## %edx = *(uint32 *) (8 * %edx + &aes_ppro_table1)
movl aes_ppro_table1(,%edx,8),%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 859:   inplace p03 &= 0xff000000
## p03#11 &= 0xff000000
## int32#3 &= 0xff000000
## %edx &= 0xff000000
and $0xff000000,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 860:   inplace a1 ^= p03
## a1#11 ^= p03#11
## int32#5 ^= int32#3
## %esi ^= %edx
xor %edx,%esi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 861:   inplace unsigned b2 >>= 16
## unsigned b2#20 >>= 16
## unsigned int32#4 >>= 16
## unsigned %ebx >>= 16
shr $16,%ebx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 862:   p23 = 0xff & (b2 >> 8)
## p23#10 = 0xff & (b2#20 >> 8)
## int32#3 = 0xff & (int32#4 >> 8)
## %edx = 0xff & (%ebx >> 8)
movzbl %bh,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 863:   p23 = *(uint32 *) (&aes_ppro_table3 + 8 * p23)
## p23#11 = *(uint32 *) (8 * p23#10 + &aes_ppro_table3)
## int32#3 = *(uint32 *) (8 * int32#3 + &aes_ppro_table3)
## %edx = *(uint32 *) (8 * %edx + &aes_ppro_table3)
movl aes_ppro_table3(,%edx,8),%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 864:   inplace p23 &= 0xff00
## p23#11 &= 0xff00
## int32#3 &= 0xff00
## %edx &= 0xff00
and $0xff00,%edx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 865:   inplace a1 ^= p23
## a1#11 ^= p23#11
## int32#5 ^= int32#3
## %esi ^= %edx
xor %edx,%esi
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 866:   p22 = 0xff & b2
## p22#10 = 0xff & b2#20
## int32#3 = 0xff & int32#4
## %edx = 0xff & %ebx
movzbl %bl,%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 867:   b2 = *(uint32 *) (&aes_ppro_table2 + 8 * p22)
## b2#21 = *(uint32 *) (8 * p22#10 + &aes_ppro_table2)
## int32#3 = *(uint32 *) (8 * int32#3 + &aes_ppro_table2)
## %edx = *(uint32 *) (8 * %edx + &aes_ppro_table2)
movl aes_ppro_table2(,%edx,8),%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 868:   inplace b2 &= 0xff
## b2#21 &= 0xff
## int32#3 &= 0xff
## %edx &= 0xff
and $0xff,%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 869:   p02 = 0xff & b1
## p02#10 = 0xff & b1#11
## int32#4 = 0xff & int32#1
## %ebx = 0xff & %eax
movzbl %al,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 870:   p02 = *(uint32 *) (&aes_ppro_table0 + 8 * p02)
## p02#11 = *(uint32 *) (8 * p02#10 + &aes_ppro_table0)
## int32#4 = *(uint32 *) (8 * int32#4 + &aes_ppro_table0)
## %ebx = *(uint32 *) (8 * %ebx + &aes_ppro_table0)
movl aes_ppro_table0(,%ebx,8),%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 871:   inplace p02 &= 0xff0000
## p02#11 &= 0xff0000
## int32#4 &= 0xff0000
## %ebx &= 0xff0000
and $0xff0000,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 872:   inplace b2 ^= p02
## b2#21 ^= p02#11
## int32#3 ^= int32#4
## %edx ^= %ebx
xor %ebx,%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 873:   p31 = 0xff & (b3 >> 8)
## p31#10 = 0xff & (b3#11 >> 8)
## int32#4 = 0xff & (int32#2 >> 8)
## %ebx = 0xff & (%ecx >> 8)
movzbl %ch,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 874:   p31 = *(uint32 *) (&aes_ppro_table3 + 8 * p31)
## p31#11 = *(uint32 *) (8 * p31#10 + &aes_ppro_table3)
## int32#4 = *(uint32 *) (8 * int32#4 + &aes_ppro_table3)
## %ebx = *(uint32 *) (8 * %ebx + &aes_ppro_table3)
movl aes_ppro_table3(,%ebx,8),%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 875:   inplace p31 &= 0xff00
## p31#11 &= 0xff00
## int32#4 &= 0xff00
## %ebx &= 0xff00
and $0xff00,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 876:   inplace b2 ^= p31
## b2#21 ^= p31#11
## int32#3 ^= int32#4
## %edx ^= %ebx
xor %ebx,%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 877:   inplace unsigned b3 >>= 16
## unsigned b3#11 >>= 16
## unsigned int32#2 >>= 16
## unsigned %ecx >>= 16
shr $16,%ecx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 878:   inplace unsigned b1 >>= 16
## unsigned b1#11 >>= 16
## unsigned int32#1 >>= 16
## unsigned %eax >>= 16
shr $16,%eax
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 879:   p33 = 0xff & (b3 >> 8)
## p33#10 = 0xff & (b3#11 >> 8)
## int32#4 = 0xff & (int32#2 >> 8)
## %ebx = 0xff & (%ecx >> 8)
movzbl %ch,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 880:   p33 = *(uint32 *) (&aes_ppro_table1 + 8 * p33)
## p33#11 = *(uint32 *) (8 * p33#10 + &aes_ppro_table1)
## int32#4 = *(uint32 *) (8 * int32#4 + &aes_ppro_table1)
## %ebx = *(uint32 *) (8 * %ebx + &aes_ppro_table1)
movl aes_ppro_table1(,%ebx,8),%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 881:   inplace p33 &= 0xff000000
## p33#11 &= 0xff000000
## int32#4 &= 0xff000000
## %ebx &= 0xff000000
and $0xff000000,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 882:   inplace a0 ^= p33
## a0#11 ^= p33#11
## int32#7 ^= int32#4
## %ebp ^= %ebx
xor %ebx,%ebp
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 883:   p13 = 0xff & (b1 >> 8)
## p13#10 = 0xff & (b1#11 >> 8)
## int32#4 = 0xff & (int32#1 >> 8)
## %ebx = 0xff & (%eax >> 8)
movzbl %ah,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 884:   p13 = *(uint32 *) (&aes_ppro_table1 + 8 * p13)
## p13#11 = *(uint32 *) (8 * p13#10 + &aes_ppro_table1)
## int32#4 = *(uint32 *) (8 * int32#4 + &aes_ppro_table1)
## %ebx = *(uint32 *) (8 * %ebx + &aes_ppro_table1)
movl aes_ppro_table1(,%ebx,8),%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 885:   inplace p13 &= 0xff000000
## p13#11 &= 0xff000000
## int32#4 &= 0xff000000
## %ebx &= 0xff000000
and $0xff000000,%ebx
## live mem32 values: 10
## live int32 values: 7
## live flags values: 0

## input line 886:   inplace b2 ^= p13
## b2#21 ^= p13#11
## int32#3 ^= int32#4
## %edx ^= %ebx
xor %ebx,%edx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 887:   p32 = 0xff & b3
## p32#10 = 0xff & b3#11
## int32#2 = 0xff & int32#2
## %ecx = 0xff & %ecx
movzbl %cl,%ecx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 888:   p32 = *(uint32 *) (&aes_ppro_table0 + 8 * p32)
## p32#11 = *(uint32 *) (8 * p32#10 + &aes_ppro_table0)
## int32#2 = *(uint32 *) (8 * int32#2 + &aes_ppro_table0)
## %ecx = *(uint32 *) (8 * %ecx + &aes_ppro_table0)
movl aes_ppro_table0(,%ecx,8),%ecx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 889:   inplace p32 &= 0xff0000
## p32#11 &= 0xff0000
## int32#2 &= 0xff0000
## %ecx &= 0xff0000
and $0xff0000,%ecx
## live mem32 values: 10
## live int32 values: 6
## live flags values: 0

## input line 890:   inplace a1 ^= p32
## a1#11 ^= p32#11
## int32#5 ^= int32#2
## %esi ^= %ecx
xor %ecx,%esi
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 891:   p12 = 0xff & b1
## p12#10 = 0xff & b1#11
## int32#1 = 0xff & int32#1
## %eax = 0xff & %eax
movzbl %al,%eax
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 892:   p12 = *(uint32 *) (&aes_ppro_table0 + 8 * p12)
## p12#11 = *(uint32 *) (8 * p12#10 + &aes_ppro_table0)
## int32#1 = *(uint32 *) (8 * int32#1 + &aes_ppro_table0)
## %eax = *(uint32 *) (8 * %eax + &aes_ppro_table0)
movl aes_ppro_table0(,%eax,8),%eax
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 893:   inplace p12 &= 0xff0000
## p12#11 &= 0xff0000
## int32#1 &= 0xff0000
## %eax &= 0xff0000
and $0xff0000,%eax
## live mem32 values: 10
## live int32 values: 5
## live flags values: 0

## input line 894:   inplace a3 ^= p12
## a3#11 ^= p12#11
## int32#6 ^= int32#1
## %edi ^= %eax
xor %eax,%edi
## live mem32 values: 10
## live int32 values: 4
## live flags values: 0

## input line 895:   inplace a0 ^= r40
xorl 164(%esp),%ebp
## live mem32 values: 9
## live int32 values: 4
## live flags values: 0

## input line 896:   inplace b2 ^= r42
xorl 172(%esp),%edx
## live mem32 values: 8
## live int32 values: 4
## live flags values: 0

## input line 897:   inplace a1 ^= r41
xorl 168(%esp),%esi
## live mem32 values: 7
## live int32 values: 4
## live flags values: 0

## input line 898:   inplace a3 ^= r43
xorl 176(%esp),%edi
## live mem32 values: 6
## live int32 values: 4
## live flags values: 0

## input line 899:   load callerint stackoffset
movl 16(%esp),%eax
## live mem32 values: 5
## live int32 values: 5
## live flags values: 0

## input line 900:   load out
movl 4(%esp,%eax),%ecx
## live mem32 values: 4
## live int32 values: 6
## live flags values: 0

## input line 901:   *(uint32 *) (out + 0) = a0
## *(uint32 *) (out + 0) = a0#11
## *(uint32 *) (int32#2 + 0) = int32#7
## *(uint32 *) (%ecx + 0) = %ebp
movl %ebp,0(%ecx)
## live mem32 values: 4
## live int32 values: 5
## live flags values: 0

## input line 902:   *(uint32 *) (out + 8) = b2
## *(uint32 *) (out + 8) = b2#21
## *(uint32 *) (int32#2 + 8) = int32#3
## *(uint32 *) (%ecx + 8) = %edx
movl %edx,8(%ecx)
## live mem32 values: 4
## live int32 values: 4
## live flags values: 0

## input line 903:   *(uint32 *) (out + 4) = a1
## *(uint32 *) (out + 4) = a1#11
## *(uint32 *) (int32#2 + 4) = int32#5
## *(uint32 *) (%ecx + 4) = %esi
movl %esi,4(%ecx)
## live mem32 values: 4
## live int32 values: 3
## live flags values: 0

## input line 904:   *(uint32 *) (out + 12) = a3
## *(uint32 *) (out + 12) = a3#11
## *(uint32 *) (int32#2 + 12) = int32#6
## *(uint32 *) (%ecx + 12) = %edi
movl %edi,12(%ecx)
## live mem32 values: 4
## live int32 values: 1
## live flags values: 0

## input line 905:   load callerint ebx
movl 0(%esp),%ebx
## live mem32 values: 3
## live int32 values: 2
## live flags values: 0

## input line 906:   load callerint esi
movl 4(%esp),%esi
## live mem32 values: 2
## live int32 values: 3
## live flags values: 0

## input line 907:   load callerint edi
movl 8(%esp),%edi
## live mem32 values: 1
## live int32 values: 4
## live flags values: 0

## input line 908:   load callerint ebp
movl 12(%esp),%ebp
## live mem32 values: 0
## live int32 values: 5
## live flags values: 0

## input line 909:   leave
add %eax,%esp
ret
## live mem32 values: 0
## live int32 values: 4
## live flags values: 0
