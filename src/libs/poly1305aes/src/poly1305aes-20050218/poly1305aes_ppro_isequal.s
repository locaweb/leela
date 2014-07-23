# poly1305aes_ppro_isequal.s version 20050213
# D. J. Bernstein
# Public domain.

# translated by qhasm-x86 version 20050213

## input line 1: register int32 x

## input line 2: register int32 y

## input line 3: register int32 d

## input line 4: register int32 x1

## input line 5: register int32 x2

## input line 6: register int32 x3

## input line 7: register int32 result

## input line 8: 

## input line 9: enter poly1305aes_ppro_isequal
.text
.p2align 5
.globl _poly1305aes_ppro_isequal
.globl poly1305aes_ppro_isequal
_poly1305aes_ppro_isequal:
poly1305aes_ppro_isequal:
sub $16,%esp
## live mem32 values: 2
## live int32 values: 4
## live flags values: 0

## input line 10: input x

## input line 11: input y

## input line 12: 

## input line 13:   store callerint ebx
movl %ebx,0(%esp)
## live mem32 values: 3
## live int32 values: 3
## live flags values: 0

## input line 14: 

## input line 15:   load x
movl 20(%esp),%eax
## live mem32 values: 2
## live int32 values: 4
## live flags values: 0

## input line 16:   load y
movl 24(%esp),%ecx
## live mem32 values: 1
## live int32 values: 5
## live flags values: 0

## input line 17: 

## input line 18:   d = *(uint32 *) (x + 0)
## d = *(uint32 *) (x + 0)
## int32#3 = *(uint32 *) (int32#1 + 0)
## %edx = *(uint32 *) (%eax + 0)
movl 0(%eax),%edx
## live mem32 values: 1
## live int32 values: 6
## live flags values: 0

## input line 19:   inplace d ^= *(uint32 *) (y + 0)
## d ^= *(uint32 *) (y + 0)
## int32#3 ^= *(uint32 *) (int32#2 + 0)
## %edx ^= *(uint32 *) (%ecx + 0)
xorl 0(%ecx),%edx
## live mem32 values: 1
## live int32 values: 6
## live flags values: 0

## input line 20: 

## input line 21:   x1 = *(uint32 *) (x + 4)
## x1 = *(uint32 *) (x + 4)
## int32#4 = *(uint32 *) (int32#1 + 4)
## %ebx = *(uint32 *) (%eax + 4)
movl 4(%eax),%ebx
## live mem32 values: 1
## live int32 values: 7
## live flags values: 0

## input line 22:   inplace x1 ^= *(uint32 *) (y + 4)
## x1 ^= *(uint32 *) (y + 4)
## int32#4 ^= *(uint32 *) (int32#2 + 4)
## %ebx ^= *(uint32 *) (%ecx + 4)
xorl 4(%ecx),%ebx
## live mem32 values: 1
## live int32 values: 7
## live flags values: 0

## input line 23:   inplace d |= x1
## d |= x1
## int32#3 |= int32#4
## %edx |= %ebx
or %ebx,%edx
## live mem32 values: 1
## live int32 values: 6
## live flags values: 0

## input line 24: 

## input line 25:   x2 = *(uint32 *) (x + 8)
## x2 = *(uint32 *) (x + 8)
## int32#4 = *(uint32 *) (int32#1 + 8)
## %ebx = *(uint32 *) (%eax + 8)
movl 8(%eax),%ebx
## live mem32 values: 1
## live int32 values: 7
## live flags values: 0

## input line 26:   inplace x2 ^= *(uint32 *) (y + 8)
## x2 ^= *(uint32 *) (y + 8)
## int32#4 ^= *(uint32 *) (int32#2 + 8)
## %ebx ^= *(uint32 *) (%ecx + 8)
xorl 8(%ecx),%ebx
## live mem32 values: 1
## live int32 values: 7
## live flags values: 0

## input line 27:   inplace d |= x2
## d |= x2
## int32#3 |= int32#4
## %edx |= %ebx
or %ebx,%edx
## live mem32 values: 1
## live int32 values: 6
## live flags values: 0

## input line 28: 

## input line 29:   x3 = *(uint32 *) (x + 12)
## x3 = *(uint32 *) (x + 12)
## int32#1 = *(uint32 *) (int32#1 + 12)
## %eax = *(uint32 *) (%eax + 12)
movl 12(%eax),%eax
## live mem32 values: 1
## live int32 values: 6
## live flags values: 0

## input line 30:   inplace x3 ^= *(uint32 *) (y + 12)
## x3 ^= *(uint32 *) (y + 12)
## int32#1 ^= *(uint32 *) (int32#2 + 12)
## %eax ^= *(uint32 *) (%ecx + 12)
xorl 12(%ecx),%eax
## live mem32 values: 1
## live int32 values: 5
## live flags values: 0

## input line 31:   inplace d |= x3
## d |= x3
## int32#3 |= int32#1
## %edx |= %eax
or %eax,%edx
## live mem32 values: 1
## live int32 values: 4
## live flags values: 0

## input line 32: 

## input line 33:   d -= 1
## d -= 1
## int32#3 -= 1
## %edx -= 1
sub $1,%edx
## live mem32 values: 1
## live int32 values: 4
## live flags values: 1

## input line 34:   

## input line 35:   d += 1
## d += 1
## int32#3 += 1
## %edx += 1
add $1,%edx
## live mem32 values: 1
## live int32 values: 3
## live flags values: 1

## input line 36:   result = 0
## result = 0
## int32#1 = 0
## %eax = 0
mov $0,%eax
## live mem32 values: 1
## live int32 values: 4
## live flags values: 1

## input line 37: 

## input line 38:   kill d

## input line 39: 

## input line 40:   carry result += result + carry
## carry result += result + carry
## carry int32#1 += int32#1 + carry
## carry %eax += %eax + carry
adc %eax,%eax
## live mem32 values: 1
## live int32 values: 4
## live flags values: 0

## input line 41: 

## input line 42:   load callerint ebx
movl 0(%esp),%ebx
## live mem32 values: 0
## live int32 values: 5
## live flags values: 0

## input line 43: 

## input line 44: output result

## input line 45: leave
add $16,%esp
ret
## live mem32 values: 0
## live int32 values: 5
## live flags values: 0
