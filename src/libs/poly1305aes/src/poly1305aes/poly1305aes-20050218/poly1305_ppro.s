# poly1305_ppro.s version 20050213
# D. J. Bernstein
# Public domain.

# translated by qhasm-x86 version 20050213

## input line 1: register int32 out

## input line 2: register int32 r

## input line 3: register int32 s

## input line 4: register int32 m

## input line 5: register int32 l

## input line 6: register int32 m0

## input line 7: register int32 m1

## input line 8: register int32 m2

## input line 9: register int32 m3

## input line 10: register float80 a0

## input line 11: register float80 a1

## input line 12: register float80 a2

## input line 13: register float80 a3

## input line 14: register float80 h0

## input line 15: register float80 h1

## input line 16: register float80 h2

## input line 17: register float80 h3

## input line 18: register float80 x0

## input line 19: register float80 x1

## input line 20: register float80 x2

## input line 21: register float80 x3

## input line 22: register float80 y0

## input line 23: register float80 y1

## input line 24: register float80 y2

## input line 25: register float80 y3

## input line 26: register float80 r0x0

## input line 27: register float80 r1x0

## input line 28: register float80 r2x0

## input line 29: register float80 r3x0

## input line 30: register float80 r0x1

## input line 31: register float80 r1x1

## input line 32: register float80 r2x1

## input line 33: register float80 sr3x1

## input line 34: register float80 r0x2

## input line 35: register float80 r1x2

## input line 36: register float80 sr2x2

## input line 37: register float80 sr3x2

## input line 38: register float80 r0x3

## input line 39: register float80 sr1x3

## input line 40: register float80 sr2x3

## input line 41: register float80 sr3x3

## input line 42: temporary mem64 d0

## input line 43: temporary mem64 d1

## input line 44: temporary mem64 d2

## input line 45: temporary mem64 d3

## input line 46: temporary mem64 r0

## input line 47: temporary mem64 r1

## input line 48: temporary mem64 r2

## input line 49: temporary mem64 r3

## input line 50: temporary mem64 sr1

## input line 51: temporary mem64 sr2

## input line 52: temporary mem64 sr3

## input line 53: 

## input line 54: stackstrategy 4096 &poly1305_ppro_constants

## input line 55: 

## input line 56: enter poly1305_ppro
.text
.p2align 5
.globl _poly1305_ppro
.globl poly1305_ppro
_poly1305_ppro:
poly1305_ppro:
mov %esp,%eax
sub $poly1305_ppro_constants,%eax
and $4095,%eax
add $144,%eax
sub %eax,%esp
## fp stack: 
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 57: input out

## input line 58: input r

## input line 59: input s

## input line 60: input m

## input line 61: input l

## input line 62: 

## input line 63:   round *(uint16 *) &poly1305_ppro_rounding
fldcw poly1305_ppro_rounding
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 64: 

## input line 65:   store callerint ebx
movl %ebx,104(%esp)
## fp stack: 
## live mem32 values: 6
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 66:   store callerint esi
movl %esi,108(%esp)
## fp stack: 
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 67:   store callerint edi
movl %edi,112(%esp)
## fp stack: 
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 68:   store callerint ebp
movl %ebp,116(%esp)
## fp stack: 
## live mem32 values: 9
## live mem64 values: 0
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 69: 

## input line 70:   load r
movl 8(%esp,%eax),%ecx
## fp stack: 
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 71: 

## input line 72:   a0 = *(int32 *) (r + 0)
## a0 = *(int32 *) (r + 0)
## fpstack#0 = *(int32 *) (int32#2 + 0)
## %st(0) = *(int32 *) (%ecx + 0)
fildl 0(%ecx)
## fp stack:  a0
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 73:   r0 = a0
fstpl 48(%esp)
## fp stack: 
## live mem32 values: 8
## live mem64 values: 1
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 74: 

## input line 75:   a1 = *(int32 *) (r + 4)
## a1 = *(int32 *) (r + 4)
## fpstack#0 = *(int32 *) (int32#2 + 4)
## %st(0) = *(int32 *) (%ecx + 4)
fildl 4(%ecx)
## fp stack:  a1
## live mem32 values: 8
## live mem64 values: 1
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 76:   a1 *= *(float64 *) &poly1305_ppro_two32
## a1 *= *(float64 *) (&poly1305_ppro_two32 + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_two32 + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_two32 + 0)
fmull 0+poly1305_ppro_two32
## live mem32 values: 8
## live mem64 values: 1
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 77:   r1 = a1
fstl 56(%esp)
## fp stack:  a1
## live mem32 values: 8
## live mem64 values: 2
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 78:   a1 *= *(float64 *) &poly1305_ppro_scale
## a1 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 8
## live mem64 values: 2
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 79:   sr1 = a1
fstpl 80(%esp)
## fp stack: 
## live mem32 values: 8
## live mem64 values: 3
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 80: 

## input line 81:   a2 = *(int32 *) (r + 8)
## a2 = *(int32 *) (r + 8)
## fpstack#0 = *(int32 *) (int32#2 + 8)
## %st(0) = *(int32 *) (%ecx + 8)
fildl 8(%ecx)
## fp stack:  a2
## live mem32 values: 8
## live mem64 values: 3
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 82:   a2 *= *(float64 *) &poly1305_ppro_two64
## a2 *= *(float64 *) (&poly1305_ppro_two64 + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_two64 + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_two64 + 0)
fmull 0+poly1305_ppro_two64
## live mem32 values: 8
## live mem64 values: 3
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 83:   r2 = a2
fstl 64(%esp)
## fp stack:  a2
## live mem32 values: 8
## live mem64 values: 4
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 84:   a2 *= *(float64 *) &poly1305_ppro_scale
## a2 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 8
## live mem64 values: 4
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 85:   sr2 = a2
fstpl 88(%esp)
## fp stack: 
## live mem32 values: 8
## live mem64 values: 5
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 86: 

## input line 87:   a3 = *(int32 *) (r + 12)
## a3 = *(int32 *) (r + 12)
## fpstack#0 = *(int32 *) (int32#2 + 12)
## %st(0) = *(int32 *) (%ecx + 12)
fildl 12(%ecx)
## fp stack:  a3
## live mem32 values: 8
## live mem64 values: 5
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 88:   a3 *= *(float64 *) &poly1305_ppro_two96
## a3 *= *(float64 *) (&poly1305_ppro_two96 + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_two96 + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_two96 + 0)
fmull 0+poly1305_ppro_two96
## live mem32 values: 8
## live mem64 values: 5
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 89:   r3 = a3
fstl 72(%esp)
## fp stack:  a3
## live mem32 values: 8
## live mem64 values: 6
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 90:   a3 *= *(float64 *) &poly1305_ppro_scale
## a3 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 8
## live mem64 values: 6
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 91:   sr3 = a3
fstpl 96(%esp)
## fp stack: 
## live mem32 values: 8
## live mem64 values: 7
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 92: 

## input line 93:   kill a0

## input line 94:   kill a1

## input line 95:   kill a2

## input line 96:   kill a3

## input line 97:   kill r

## input line 98: 

## input line 99:   h3 = 0
## h3 = 0
## fpstack#0 = 0
## %st(0) = 0
fldz
## fp stack:  h3
## live mem32 values: 8
## live mem64 values: 7
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 100:   h2 = 0
## h2 = 0
## fpstack#0 = 0
## %st(0) = 0
fldz
## fp stack:  h3 h2
## live mem32 values: 8
## live mem64 values: 7
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 2
## live flags values: 0

## input line 101:   h1 = 0
## h1 = 0
## fpstack#0 = 0
## %st(0) = 0
fldz
## fp stack:  h3 h2 h1
## live mem32 values: 8
## live mem64 values: 7
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 3
## live flags values: 0

## input line 102:   h0 = 0
## h0 = 0
## fpstack#0 = 0
## %st(0) = 0
fldz
## fp stack:  h3 h2 h1 h0
## live mem32 values: 8
## live mem64 values: 7
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 103: 

## input line 104:   d0 top = 0x43300000
movl $0x43300000,20(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 8
## live mem64 values: 8
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 105:   d1 top = 0x45300000
movl $0x45300000,28(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 8
## live mem64 values: 9
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 106:   d2 top = 0x47300000
movl $0x47300000,36(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 8
## live mem64 values: 10
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 107:   d3 top = 0x49300000
movl $0x49300000,44(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 8
## live mem64 values: 11
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 108: 

## input line 109:   load m
movl 16(%esp,%eax),%esi
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 110:   load l
movl 20(%esp,%eax),%ecx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 6
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 111: 

## input line 112:   store callerint stackoffset
movl %eax,120(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 113: 

## input line 114: flags l - 16
## flags l - 16
## flags int32#2 - 16
## flags %ecx - 16
cmp $16,%ecx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 1

## input line 115: goto addatmost15bytes if unsigned <
## fp stack:  h3 h2 h1 h0
## fp stack unchanged by jump
jb ._addatmost15bytes

## input line 116: 

## input line 117:   m3 = *(uint32 *) (m + 12)
## m3 = *(uint32 *) (m + 12)
## int32#6 = *(uint32 *) (int32#5 + 12)
## %edi = *(uint32 *) (%esi + 12)
movl 12(%esi),%edi
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 118:   m2 = *(uint32 *) (m + 8)
## m2 = *(uint32 *) (m + 8)
## int32#4 = *(uint32 *) (int32#5 + 8)
## %ebx = *(uint32 *) (%esi + 8)
movl 8(%esi),%ebx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 119:   m1 = *(uint32 *) (m + 4)
## m1 = *(uint32 *) (m + 4)
## int32#3 = *(uint32 *) (int32#5 + 4)
## %edx = *(uint32 *) (%esi + 4)
movl 4(%esi),%edx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 120:   m0 = *(uint32 *) (m + 0)
## m0 = *(uint32 *) (m + 0)
## int32#1 = *(uint32 *) (int32#5 + 0)
## %eax = *(uint32 *) (%esi + 0)
movl 0(%esi),%eax
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 121:   inplace d3 bottom = m3
movl %edi,40(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 122:   inplace d2 bottom = m2
movl %ebx,32(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 123:   inplace d1 bottom = m1
movl %edx,24(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 124:   inplace d0 bottom = m0
movl %eax,16(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 125: 

## input line 126:   kill m0

## input line 127:   kill m1

## input line 128:   kill m2

## input line 129:   kill m3

## input line 130: 

## input line 131:   m += 16
## m += 16
## int32#5 += 16
## %esi += 16
add $16,%esi
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 132:   l -= 16
## l -= 16
## int32#2 -= 16
## %ecx -= 16
sub $16,%ecx
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 133: 

## input line 134:   h3 += d3
fxch %st(3)
faddl 40(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 135:   h3 -= *(float64 *) &poly1305_ppro_doffset3minustwo128
## h3 -= *(float64 *) (&poly1305_ppro_doffset3minustwo128 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset3minustwo128 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset3minustwo128 + 0)
fsubl 0+poly1305_ppro_doffset3minustwo128
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 136:   h2 += d2
fxch %st(2)
faddl 32(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 137:   h2 -= *(float64 *) &poly1305_ppro_doffset2
## h2 -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
fsubl 0+poly1305_ppro_doffset2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 138:   h1 += d1
fxch %st(1)
faddl 24(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 139:   h1 -= *(float64 *) &poly1305_ppro_doffset1
## h1 -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
fsubl 0+poly1305_ppro_doffset1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 140:   h0 += d0
fxch %st(3)
faddl 16(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 141:   h0 -= *(float64 *) &poly1305_ppro_doffset0
## h0 -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
fsubl 0+poly1305_ppro_doffset0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 142: 

## input line 143: flags l - 16
## flags l - 16
## flags int32#2 - 16
## flags %ecx - 16
cmp $16,%ecx
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 1

## input line 144: goto multiplyaddatmost15bytes if unsigned <
## fp stack:  h1 h3 h2 h0
## fp stack unchanged by jump
jb ._multiplyaddatmost15bytes

## input line 145: 

## input line 146: multiplyaddatleast16bytes
._multiplyaddatleast16bytes:

## input line 147: 

## input line 148:   m3 = *(uint32 *) (m + 12)
## m3#2 = *(uint32 *) (m + 12)
## int32#6 = *(uint32 *) (int32#5 + 12)
## %edi = *(uint32 *) (%esi + 12)
movl 12(%esi),%edi
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 149:   m2 = *(uint32 *) (m + 8)
## m2#2 = *(uint32 *) (m + 8)
## int32#4 = *(uint32 *) (int32#5 + 8)
## %ebx = *(uint32 *) (%esi + 8)
movl 8(%esi),%ebx
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 150:   m1 = *(uint32 *) (m + 4)
## m1#2 = *(uint32 *) (m + 4)
## int32#3 = *(uint32 *) (int32#5 + 4)
## %edx = *(uint32 *) (%esi + 4)
movl 4(%esi),%edx
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 151:   m0 = *(uint32 *) (m + 0)
## m0#2 = *(uint32 *) (m + 0)
## int32#1 = *(uint32 *) (int32#5 + 0)
## %eax = *(uint32 *) (%esi + 0)
movl 0(%esi),%eax
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 152:   inplace d3 bottom = m3
movl %edi,40(%esp)
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 153:   inplace d2 bottom = m2
movl %ebx,32(%esp)
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 154:   inplace d1 bottom = m1
movl %edx,24(%esp)
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 155:   inplace d0 bottom = m0
movl %eax,16(%esp)
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 156: 

## input line 157:   kill m0

## input line 158:   kill m1

## input line 159:   kill m2

## input line 160:   kill m3

## input line 161: 

## input line 162:   m += 16
## m += 16
## int32#5 += 16
## %esi += 16
add $16,%esi
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 163:   l -= 16
## l -= 16
## int32#2 -= 16
## %ecx -= 16
sub $16,%ecx
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 164: 

## input line 165:   x0 = *(float64 *) &poly1305_ppro_alpha130
## x0 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha130 + 0)
fldl 0+poly1305_ppro_alpha130
## fp stack:  h1 h3 h2 h0 x0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 166:   x0 += h3
fadd %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 167:   x0 -= *(float64 *) &poly1305_ppro_alpha130
## x0 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
fsubl 0+poly1305_ppro_alpha130
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 168:   h3 -= x0
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(3)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 169:   x0 *= *(float64 *) &poly1305_ppro_scale
## x0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 170: 

## input line 171:   x1 = *(float64 *) &poly1305_ppro_alpha32
## x1 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha32 + 0)
fldl 0+poly1305_ppro_alpha32
## fp stack:  h1 h3 h2 h0 x0 x1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 172:   x1 += h0
fadd %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 173:   x1 -= *(float64 *) &poly1305_ppro_alpha32
## x1 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
fsubl 0+poly1305_ppro_alpha32
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 174:   h0 -= x1
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(2)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 175: 

## input line 176:   x0 += h0
fxch %st(2)
faddp %st(0),%st(1)
## fp stack:  h1 h3 h2 x1 x0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 177: 

## input line 178:   x2 = *(float64 *) &poly1305_ppro_alpha64
## x2 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha64 + 0)
fldl 0+poly1305_ppro_alpha64
## fp stack:  h1 h3 h2 x1 x0 x2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 179:   x2 += h1
fadd %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 180:   x2 -= *(float64 *) &poly1305_ppro_alpha64
## x2 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
fsubl 0+poly1305_ppro_alpha64
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 181:   h1 -= x2
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(5)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 182: 

## input line 183:   x3 = *(float64 *) &poly1305_ppro_alpha96
## x3 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha96 + 0)
fldl 0+poly1305_ppro_alpha96
## fp stack:  h1 h3 h2 x1 x0 x2 x3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 184:   x3 += h2
fadd %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 185:   x3 -= *(float64 *) &poly1305_ppro_alpha96
## x3 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
fsubl 0+poly1305_ppro_alpha96
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 186:   h2 -= x3
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(4)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 187: 

## input line 188:   x2 += h2
fxch %st(4)
faddp %st(0),%st(1)
## fp stack:  h1 h3 x3 x1 x0 x2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 189:   x3 += h3
fxch %st(4)
faddp %st(0),%st(3)
## fp stack:  h1 x2 x3 x1 x0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 190:   x1 += h1
fxch %st(4)
faddp %st(0),%st(1)
## fp stack:  x0 x2 x3 x1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 191: 

## input line 192:   kill h0

## input line 193:   kill h1

## input line 194:   kill h2

## input line 195:   kill h3

## input line 196: 

## input line 197:   h3 = r3
fldl 72(%esp)
## fp stack:  x0 x2 x3 x1 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 198:   h3 *= x0
fmul %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 199:   h2 = r2
fldl 64(%esp)
## fp stack:  x0 x2 x3 x1 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 200:   h2 *= x0
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 201:   h1 = r1
fldl 56(%esp)
## fp stack:  x0 x2 x3 x1 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 202:   h1 *= x0
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 203:   h0 = r0
fldl 48(%esp)
## fp stack:  x0 x2 x3 x1 h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 204:   h0 *= x0
fmulp %st(0),%st(7)
## fp stack:  h0 x2 x3 x1 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 205: 

## input line 206:   r2x1 = r2
fldl 64(%esp)
## fp stack:  h0 x2 x3 x1 h3 h2 h1 r2x1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 207:   r2x1 *= x1
fmul %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 208:   h3 += r2x1
faddp %st(0),%st(3)
## fp stack:  h0 x2 x3 x1 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 209:   r1x1 = r1
fldl 56(%esp)
## fp stack:  h0 x2 x3 x1 h3 h2 h1 r1x1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 210:   r1x1 *= x1
fmul %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 211:   h2 += r1x1
faddp %st(0),%st(2)
## fp stack:  h0 x2 x3 x1 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 212:   r0x1 = r0
fldl 48(%esp)
## fp stack:  h0 x2 x3 x1 h3 h2 h1 r0x1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 213:   r0x1 *= x1
fmul %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 214:   h1 += r0x1
faddp %st(0),%st(1)
## fp stack:  h0 x2 x3 x1 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 215:   sr3x1 = sr3
fldl 96(%esp)
## fp stack:  h0 x2 x3 x1 h3 h2 h1 sr3x1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 216:   sr3x1 *= x1
fmulp %st(0),%st(4)
## fp stack:  h0 x2 x3 sr3x1 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 217:   h0 += sr3x1
fxch %st(3)
faddp %st(0),%st(6)
## fp stack:  h0 x2 x3 h1 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 218: 

## input line 219:   r1x2 = r1
fldl 56(%esp)
## fp stack:  h0 x2 x3 h1 h3 h2 r1x2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 220:   r1x2 *= x2
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 221:   h3 += r1x2
faddp %st(0),%st(2)
## fp stack:  h0 x2 x3 h1 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 222:   r0x2 = r0
fldl 48(%esp)
## fp stack:  h0 x2 x3 h1 h3 h2 r0x2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 223:   r0x2 *= x2
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 224:   h2 += r0x2
faddp %st(0),%st(1)
## fp stack:  h0 x2 x3 h1 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 225:   sr3x2 = sr3
fldl 96(%esp)
## fp stack:  h0 x2 x3 h1 h3 h2 sr3x2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 226:   sr3x2 *= x2
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 227:   h1 += sr3x2
faddp %st(0),%st(3)
## fp stack:  h0 x2 x3 h1 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 228:   sr2x2 = sr2
fldl 88(%esp)
## fp stack:  h0 x2 x3 h1 h3 h2 sr2x2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 229:   sr2x2 *= x2
fmulp %st(0),%st(5)
## fp stack:  h0 sr2x2 x3 h1 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 230:   h0 += sr2x2
fxch %st(4)
faddp %st(0),%st(5)
## fp stack:  h0 h2 x3 h1 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 231: 

## input line 232:   r0x3 = r0
fldl 48(%esp)
## fp stack:  h0 h2 x3 h1 h3 r0x3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 233:   r0x3 *= x3
fmul %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 234:   h3 += r0x3
faddp %st(0),%st(1)
## fp stack:  h0 h2 x3 h1 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 235:   stacktop h0
fxch %st(4)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 236:   sr3x3 = sr3
fldl 96(%esp)
## fp stack:  h3 h2 x3 h1 h0 sr3x3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 237:   sr3x3 *= x3
fmul %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 238:   h2 += sr3x3
faddp %st(0),%st(4)
## fp stack:  h3 h2 x3 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 239:   stacktop h1
fxch %st(1)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 240:   sr2x3 = sr2
fldl 88(%esp)
## fp stack:  h3 h2 x3 h0 h1 sr2x3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 241:   sr2x3 *= x3
fmul %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 242:   h1 += sr2x3
faddp %st(0),%st(1)
## fp stack:  h3 h2 x3 h0 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 243:   sr1x3 = sr1
fldl 80(%esp)
## fp stack:  h3 h2 x3 h0 h1 sr1x3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 244:   sr1x3 *= x3
fmulp %st(0),%st(3)
## fp stack:  h3 h2 sr1x3 h0 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 245:   h0 += sr1x3
fxch %st(2)
faddp %st(0),%st(1)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 246: 

## input line 247: flags l - 16
## flags l - 16
## flags int32#2 - 16
## flags %ecx - 16
cmp $16,%ecx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 1

## input line 248: 
## fp stack:  h3 h2 h1 h0

## input line 249:   h3 += d3
fxch %st(3)
faddl 40(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 250:   h3 -= *(float64 *) &poly1305_ppro_doffset3minustwo128
## h3 -= *(float64 *) (&poly1305_ppro_doffset3minustwo128 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset3minustwo128 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset3minustwo128 + 0)
fsubl 0+poly1305_ppro_doffset3minustwo128
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 251:   h2 += d2
fxch %st(2)
faddl 32(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 252:   h2 -= *(float64 *) &poly1305_ppro_doffset2
## h2 -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
fsubl 0+poly1305_ppro_doffset2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 253:   h1 += d1
fxch %st(1)
faddl 24(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 254:   h1 -= *(float64 *) &poly1305_ppro_doffset1
## h1 -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
fsubl 0+poly1305_ppro_doffset1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 255:   h0 += d0
fxch %st(3)
faddl 16(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 256:   h0 -= *(float64 *) &poly1305_ppro_doffset0
## h0 -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
fsubl 0+poly1305_ppro_doffset0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 257: 

## input line 258: goto multiplyaddatleast16bytes if unsigned >=
## fp stack unchanged by jump
jae ._multiplyaddatleast16bytes

## input line 259: 

## input line 260: multiplyaddatmost15bytes
## fp stack unchanged by fallthrough
._multiplyaddatmost15bytes:

## input line 261: 

## input line 262:   x0 = *(float64 *) &poly1305_ppro_alpha130
## x0#2 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha130 + 0)
fldl 0+poly1305_ppro_alpha130
## fp stack:  h1 h3 h2 h0 x0#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 263:   x0 += h3
fadd %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 264:   x0 -= *(float64 *) &poly1305_ppro_alpha130
## x0#2 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
fsubl 0+poly1305_ppro_alpha130
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 265:   h3 -= x0
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(3)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 266:   x0 *= *(float64 *) &poly1305_ppro_scale
## x0#2 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 267: 

## input line 268:   x1 = *(float64 *) &poly1305_ppro_alpha32
## x1#2 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha32 + 0)
fldl 0+poly1305_ppro_alpha32
## fp stack:  h1 h3 h2 h0 x0#2 x1#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 269:   x1 += h0
fadd %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 270:   x1 -= *(float64 *) &poly1305_ppro_alpha32
## x1#2 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
fsubl 0+poly1305_ppro_alpha32
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 271:   h0 -= x1
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(2)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 272: 

## input line 273:   x2 = *(float64 *) &poly1305_ppro_alpha64
## x2#2 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha64 + 0)
fldl 0+poly1305_ppro_alpha64
## fp stack:  h1 h3 h2 h0 x0#2 x1#2 x2#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 274:   x2 += h1
fadd %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 275:   x2 -= *(float64 *) &poly1305_ppro_alpha64
## x2#2 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
fsubl 0+poly1305_ppro_alpha64
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 276:   h1 -= x2
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(6)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 277: 

## input line 278:   x3 = *(float64 *) &poly1305_ppro_alpha96
## x3#2 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha96 + 0)
fldl 0+poly1305_ppro_alpha96
## fp stack:  h1 h3 h2 h0 x0#2 x1#2 x2#2 x3#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 279:   x3 += h2
fadd %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 280:   x3 -= *(float64 *) &poly1305_ppro_alpha96
## x3#2 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
fsubl 0+poly1305_ppro_alpha96
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 281:   h2 -= x3
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(5)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 282: 

## input line 283:   x0 += h0
fxch %st(4)
faddp %st(0),%st(3)
## fp stack:  h1 h3 h2 x3#2 x0#2 x1#2 x2#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 284:   x1 += h1
fxch %st(6)
faddp %st(0),%st(1)
## fp stack:  x2#2 h3 h2 x3#2 x0#2 x1#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 285:   x2 += h2
fxch %st(3)
faddp %st(0),%st(5)
## fp stack:  x2#2 h3 x1#2 x3#2 x0#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 286:   x3 += h3
fxch %st(3)
faddp %st(0),%st(1)
## fp stack:  x2#2 x0#2 x1#2 x3#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 287: 

## input line 288:   kill h0

## input line 289:   kill h1

## input line 290:   kill h2

## input line 291:   kill h3

## input line 292: 

## input line 293:   h3 = r3
fldl 72(%esp)
## fp stack:  x2#2 x0#2 x1#2 x3#2 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 294:   h3 *= x0
fmul %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 295:   h2 = r2
fldl 64(%esp)
## fp stack:  x2#2 x0#2 x1#2 x3#2 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 296:   h2 *= x0
fmul %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 297:   h1 = r1
fldl 56(%esp)
## fp stack:  x2#2 x0#2 x1#2 x3#2 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 298:   h1 *= x0
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 299:   h0 = r0
fldl 48(%esp)
## fp stack:  x2#2 x0#2 x1#2 x3#2 h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 300:   h0 *= x0
fmulp %st(0),%st(6)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 301: 

## input line 302:   r2x1 = r2
fldl 64(%esp)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1 r2x1#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 303:   r2x1 *= x1
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 304:   h3 += r2x1
faddp %st(0),%st(3)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 305:   r1x1 = r1
fldl 56(%esp)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1 r1x1#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 306:   r1x1 *= x1
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 307:   h2 += r1x1
faddp %st(0),%st(2)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 308:   r0x1 = r0
fldl 48(%esp)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1 r0x1#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 309:   r0x1 *= x1
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 310:   h1 += r0x1
faddp %st(0),%st(1)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 311:   sr3x1 = sr3
fldl 96(%esp)
## fp stack:  x2#2 h0 x1#2 x3#2 h3 h2 h1 sr3x1#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 312:   sr3x1 *= x1
fmulp %st(0),%st(5)
## fp stack:  x2#2 h0 sr3x1#2 x3#2 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 313:   h0 += sr3x1
fxch %st(4)
faddp %st(0),%st(5)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 314: 

## input line 315:   r1x2 = r1
fldl 56(%esp)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2 r1x2#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 316:   r1x2 *= x2
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 317:   h3 += r1x2
faddp %st(0),%st(2)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 318:   r0x2 = r0
fldl 48(%esp)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2 r0x2#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 319:   r0x2 *= x2
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 320:   h2 += r0x2
faddp %st(0),%st(1)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 321:   sr3x2 = sr3
fldl 96(%esp)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2 sr3x2#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 322:   sr3x2 *= x2
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 323:   h1 += sr3x2
faddp %st(0),%st(4)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 324:   sr2x2 = sr2
fldl 88(%esp)
## fp stack:  x2#2 h0 h1 x3#2 h3 h2 sr2x2#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 325:   sr2x2 *= x2
fmulp %st(0),%st(6)
## fp stack:  sr2x2#2 h0 h1 x3#2 h3 h2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 326:   h0 += sr2x2
fxch %st(5)
faddp %st(0),%st(4)
## fp stack:  h2 h0 h1 x3#2 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 327: 

## input line 328:   r0x3 = r0
fldl 48(%esp)
## fp stack:  h2 h0 h1 x3#2 h3 r0x3#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 329:   r0x3 *= x3
fmul %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 330:   h3 += r0x3
faddp %st(0),%st(1)
## fp stack:  h2 h0 h1 x3#2 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 331:   sr3x3 = sr3
fldl 96(%esp)
## fp stack:  h2 h0 h1 x3#2 h3 sr3x3#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 332:   sr3x3 *= x3
fmul %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 333:   h2 += sr3x3
faddp %st(0),%st(5)
## fp stack:  h2 h0 h1 x3#2 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 334:   sr2x3 = sr2
fldl 88(%esp)
## fp stack:  h2 h0 h1 x3#2 h3 sr2x3#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 335:   sr2x3 *= x3
fmul %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 336:   h1 += sr2x3
faddp %st(0),%st(3)
## fp stack:  h2 h0 h1 x3#2 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 337:   sr1x3 = sr1
fldl 80(%esp)
## fp stack:  h2 h0 h1 x3#2 h3 sr1x3#2
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 338:   sr1x3 *= x3
fmulp %st(0),%st(2)
## fp stack:  h2 h0 h1 sr1x3#2 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 339:   h0 += sr1x3
fxch %st(1)
faddp %st(0),%st(3)
## fp stack:  h2 h0 h1 h3
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 340: 

## input line 341: addatmost15bytes
## automatically reorganizing fp stack for fallthrough
fxch %st(3)
## fp stack:  h3 h0 h1 h2
fxch %st(2)
## fp stack:  h3 h2 h1 h0
._addatmost15bytes:

## input line 342: 

## input line 343: flags l - 0
## flags l - 0
## flags int32#2 - 0
## flags %ecx - 0
cmp $0,%ecx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 1

## input line 344: goto nomorebytes if ==
## fp stack:  h3 h2 h1 h0
## fp stack unchanged by jump
je ._nomorebytes

## input line 345: 

## input line 346: temporary mem128 lastchunk

## input line 347: register int32 destination

## input line 348: 

## input line 349:   lastchunk lowest = 0
movl $0,0(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 350:   inplace lastchunk lower = 0
movl $0,4(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 351:   inplace lastchunk higher = 0
movl $0,8(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 352:   inplace lastchunk highest = 0
movl $0,12(%esp)
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 353: 

## input line 354:   destination = &lastchunk
leal 0(%esp),%edi
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 355:   while (l) { *destination++ = *m++; --l }
rep movsb
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 1
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 356:   *(uchar *) destination = 1
## *(uchar *) (destination + 0) = 1
## *(uchar *) (int32#6 + 0) = 1
## *(uchar *) (%edi + 0) = 1
movb $1,0(%edi)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 0
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 357:   kill destination

## input line 358: 

## input line 359:   m3 = lastchunk highest
movl 12(%esp),%ebx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 1
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 360:   m2 = lastchunk higher
movl 8(%esp),%edx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 361:   m1 = lastchunk lower
movl 4(%esp),%ecx
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 1
## live float80 values: 4
## live flags values: 0

## input line 362:   m0 = lastchunk lowest
movl 0(%esp),%eax
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 363:   inplace d3 bottom = m3
movl %ebx,40(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 364:   inplace d2 bottom = m2
movl %edx,32(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 365:   inplace d1 bottom = m1
movl %ecx,24(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 366:   inplace d0 bottom = m0
movl %eax,16(%esp)
## fp stack:  h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 11
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 367: 

## input line 368:   kill lastchunk

## input line 369:   kill m0

## input line 370:   kill m1

## input line 371:   kill m2

## input line 372:   kill m3

## input line 373: 

## input line 374:   h3 += d3
fxch %st(3)
faddl 40(%esp)
## fp stack:  h0 h2 h1 h3
## live mem32 values: 7
## live mem64 values: 10
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 375:   h3 -= *(float64 *) &poly1305_ppro_doffset3
## h3 -= *(float64 *) (&poly1305_ppro_doffset3 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset3 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset3 + 0)
fsubl 0+poly1305_ppro_doffset3
## live mem32 values: 7
## live mem64 values: 10
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 376:   h2 += d2
fxch %st(2)
faddl 32(%esp)
## fp stack:  h0 h3 h1 h2
## live mem32 values: 7
## live mem64 values: 9
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 377:   h2 -= *(float64 *) &poly1305_ppro_doffset2
## h2 -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset2 + 0)
fsubl 0+poly1305_ppro_doffset2
## live mem32 values: 7
## live mem64 values: 9
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 378:   h1 += d1
fxch %st(1)
faddl 24(%esp)
## fp stack:  h0 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 8
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 379:   h1 -= *(float64 *) &poly1305_ppro_doffset1
## h1 -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset1 + 0)
fsubl 0+poly1305_ppro_doffset1
## live mem32 values: 7
## live mem64 values: 8
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 380:   h0 += d0
fxch %st(3)
faddl 16(%esp)
## fp stack:  h1 h3 h2 h0
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 381:   h0 -= *(float64 *) &poly1305_ppro_doffset0
## h0 -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_doffset0 + 0)
fsubl 0+poly1305_ppro_doffset0
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 382: 

## input line 383:   x0 = *(float64 *) &poly1305_ppro_alpha130
## x0#3 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha130 + 0)
fldl 0+poly1305_ppro_alpha130
## fp stack:  h1 h3 h2 h0 x0#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 384:   x0 += h3
fadd %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 385:   x0 -= *(float64 *) &poly1305_ppro_alpha130
## x0#3 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
fsubl 0+poly1305_ppro_alpha130
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 386:   h3 -= x0
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(3)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 387:   x0 *= *(float64 *) &poly1305_ppro_scale
## x0#3 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 388: 

## input line 389:   x1 = *(float64 *) &poly1305_ppro_alpha32
## x1#3 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha32 + 0)
fldl 0+poly1305_ppro_alpha32
## fp stack:  h1 h3 h2 h0 x0#3 x1#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 390:   x1 += h0
fadd %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 391:   x1 -= *(float64 *) &poly1305_ppro_alpha32
## x1#3 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
fsubl 0+poly1305_ppro_alpha32
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 392:   h0 -= x1
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(2)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 393: 

## input line 394:   x2 = *(float64 *) &poly1305_ppro_alpha64
## x2#3 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha64 + 0)
fldl 0+poly1305_ppro_alpha64
## fp stack:  h1 h3 h2 h0 x0#3 x1#3 x2#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 395:   x2 += h1
fadd %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 396:   x2 -= *(float64 *) &poly1305_ppro_alpha64
## x2#3 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
fsubl 0+poly1305_ppro_alpha64
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 397:   h1 -= x2
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(6)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 398: 

## input line 399:   x3 = *(float64 *) &poly1305_ppro_alpha96
## x3#3 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha96 + 0)
fldl 0+poly1305_ppro_alpha96
## fp stack:  h1 h3 h2 h0 x0#3 x1#3 x2#3 x3#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 400:   x3 += h2
fadd %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 401:   x3 -= *(float64 *) &poly1305_ppro_alpha96
## x3#3 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
fsubl 0+poly1305_ppro_alpha96
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 402:   h2 -= x3
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(5)
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 403: 

## input line 404:   x0 += h0
fxch %st(4)
faddp %st(0),%st(3)
## fp stack:  h1 h3 h2 x3#3 x0#3 x1#3 x2#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 405:   x1 += h1
fxch %st(6)
faddp %st(0),%st(1)
## fp stack:  x2#3 h3 h2 x3#3 x0#3 x1#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 406:   x2 += h2
fxch %st(3)
faddp %st(0),%st(5)
## fp stack:  x2#3 h3 x1#3 x3#3 x0#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 407:   x3 += h3
fxch %st(3)
faddp %st(0),%st(1)
## fp stack:  x2#3 x0#3 x1#3 x3#3
## live mem32 values: 7
## live mem64 values: 7
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 408: 

## input line 409:   kill h0

## input line 410:   kill h1

## input line 411:   kill h2

## input line 412:   kill h3

## input line 413: 

## input line 414:   h3 = r3
fldl 72(%esp)
## fp stack:  x2#3 x0#3 x1#3 x3#3 h3
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 415:   h3 *= x0
fmul %st(3),%st(0)
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 416:   h2 = r2
fldl 64(%esp)
## fp stack:  x2#3 x0#3 x1#3 x3#3 h3 h2
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 417:   h2 *= x0
fmul %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 418:   h1 = r1
fldl 56(%esp)
## fp stack:  x2#3 x0#3 x1#3 x3#3 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 419:   h1 *= x0
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 420:   h0 = r0
fldl 48(%esp)
## fp stack:  x2#3 x0#3 x1#3 x3#3 h3 h2 h1 h0
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 421:   h0 *= x0
fmulp %st(0),%st(6)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 6
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 422: 

## input line 423:   r2x1 = r2
fldl 64(%esp)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1 r2x1#3
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 424:   r2x1 *= x1
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 425:   h3 += r2x1
faddp %st(0),%st(3)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 426:   r1x1 = r1
fldl 56(%esp)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1 r1x1#3
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 427:   r1x1 *= x1
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 428:   h2 += r1x1
faddp %st(0),%st(2)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 429:   r0x1 = r0
fldl 48(%esp)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1 r0x1#3
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 430:   r0x1 *= x1
fmul %st(5),%st(0)
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 431:   h1 += r0x1
faddp %st(0),%st(1)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 432:   sr3x1 = sr3
fldl 96(%esp)
## fp stack:  x2#3 h0 x1#3 x3#3 h3 h2 h1 sr3x1#3
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 433:   sr3x1 *= x1
fmulp %st(0),%st(5)
## fp stack:  x2#3 h0 sr3x1#3 x3#3 h3 h2 h1
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 434:   h0 += sr3x1
fxch %st(4)
faddp %st(0),%st(5)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2
## live mem32 values: 7
## live mem64 values: 5
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 435: 

## input line 436:   r1x2 = r1
fldl 56(%esp)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2 r1x2#3
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 437:   r1x2 *= x2
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 438:   h3 += r1x2
faddp %st(0),%st(2)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 439:   r0x2 = r0
fldl 48(%esp)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2 r0x2#3
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 440:   r0x2 *= x2
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 441:   h2 += r0x2
faddp %st(0),%st(1)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 442:   sr3x2 = sr3
fldl 96(%esp)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2 sr3x2#3
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 443:   sr3x2 *= x2
fmul %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 444:   h1 += sr3x2
faddp %st(0),%st(4)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 445:   sr2x2 = sr2
fldl 88(%esp)
## fp stack:  x2#3 h0 h1 x3#3 h3 h2 sr2x2#3
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 446:   sr2x2 *= x2
fmulp %st(0),%st(6)
## fp stack:  sr2x2#3 h0 h1 x3#3 h3 h2
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 447:   h0 += sr2x2
fxch %st(5)
faddp %st(0),%st(4)
## fp stack:  h2 h0 h1 x3#3 h3
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 448: 

## input line 449:   r0x3 = r0
fldl 48(%esp)
## fp stack:  h2 h0 h1 x3#3 h3 r0x3#3
## live mem32 values: 7
## live mem64 values: 3
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 450:   r0x3 *= x3
fmul %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 3
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 451:   h3 += r0x3
faddp %st(0),%st(1)
## fp stack:  h2 h0 h1 x3#3 h3
## live mem32 values: 7
## live mem64 values: 3
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 452:   sr3x3 = sr3
fldl 96(%esp)
## fp stack:  h2 h0 h1 x3#3 h3 sr3x3#3
## live mem32 values: 7
## live mem64 values: 2
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 453:   sr3x3 *= x3
fmul %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 2
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 454:   h2 += sr3x3
faddp %st(0),%st(5)
## fp stack:  h2 h0 h1 x3#3 h3
## live mem32 values: 7
## live mem64 values: 2
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 455:   sr2x3 = sr2
fldl 88(%esp)
## fp stack:  h2 h0 h1 x3#3 h3 sr2x3#3
## live mem32 values: 7
## live mem64 values: 1
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 456:   sr2x3 *= x3
fmul %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 1
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 457:   h1 += sr2x3
faddp %st(0),%st(3)
## fp stack:  h2 h0 h1 x3#3 h3
## live mem32 values: 7
## live mem64 values: 1
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 458:   sr1x3 = sr1
fldl 80(%esp)
## fp stack:  h2 h0 h1 x3#3 h3 sr1x3#3
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 459:   sr1x3 *= x3
fmulp %st(0),%st(2)
## fp stack:  h2 h0 h1 sr1x3#3 h3
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 460:   h0 += sr1x3
fxch %st(1)
faddp %st(0),%st(3)
## fp stack:  h2 h0 h1 h3
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 461: 

## input line 462: 

## input line 463: nomorebytes
## automatically reorganizing fp stack for fallthrough
fxch %st(3)
## fp stack:  h3 h0 h1 h2
fxch %st(2)
## fp stack:  h3 h2 h1 h0
._nomorebytes:

## input line 464: 

## input line 465:   x0 = *(float64 *) &poly1305_ppro_alpha130
## x0#4 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha130 + 0)
fldl 0+poly1305_ppro_alpha130
## fp stack:  h3 h2 h1 h0 x0#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 466:   x0 += h3
fadd %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 467:   x0 -= *(float64 *) &poly1305_ppro_alpha130
## x0#4 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha130 + 0)
fsubl 0+poly1305_ppro_alpha130
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 468:   h3 -= x0
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(4)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 469:   x0 *= *(float64 *) &poly1305_ppro_scale
## x0#4 *= *(float64 *) (&poly1305_ppro_scale + 0)
## fpstack#0 *= *(float64 *) (&poly1305_ppro_scale + 0)
## %st(0) *= *(float64 *) (&poly1305_ppro_scale + 0)
fmull 0+poly1305_ppro_scale
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 470: 

## input line 471:   x1 = *(float64 *) &poly1305_ppro_alpha32
## x1#4 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha32 + 0)
fldl 0+poly1305_ppro_alpha32
## fp stack:  h3 h2 h1 h0 x0#4 x1#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 472:   x1 += h0
fadd %st(2),%st(0)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 473:   x1 -= *(float64 *) &poly1305_ppro_alpha32
## x1#4 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha32 + 0)
fsubl 0+poly1305_ppro_alpha32
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 474:   h0 -= x1
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(2)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 475: 

## input line 476:   x2 = *(float64 *) &poly1305_ppro_alpha64
## x2#4 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha64 + 0)
fldl 0+poly1305_ppro_alpha64
## fp stack:  h3 h2 h1 h0 x0#4 x1#4 x2#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 477:   x2 += h1
fadd %st(4),%st(0)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 478:   x2 -= *(float64 *) &poly1305_ppro_alpha64
## x2#4 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha64 + 0)
fsubl 0+poly1305_ppro_alpha64
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 479:   h1 -= x2
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(4)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 480: 

## input line 481:   x3 = *(float64 *) &poly1305_ppro_alpha96
## x3#4 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 = *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) = *(float64 *) (&poly1305_ppro_alpha96 + 0)
fldl 0+poly1305_ppro_alpha96
## fp stack:  h3 h2 h1 h0 x0#4 x1#4 x2#4 x3#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 482:   x3 += h2
fadd %st(6),%st(0)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 483:   x3 -= *(float64 *) &poly1305_ppro_alpha96
## x3#4 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## fpstack#0 -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
## %st(0) -= *(float64 *) (&poly1305_ppro_alpha96 + 0)
fsubl 0+poly1305_ppro_alpha96
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 484:   h2 -= x3
## see gcc documentation for discussion of why this is not fsub
fsubr %st(0),%st(6)
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 8
## live flags values: 0

## input line 485: 

## input line 486:   x0 += h0
fxch %st(4)
faddp %st(0),%st(3)
## fp stack:  h3 h2 h1 x3#4 x0#4 x1#4 x2#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 7
## live flags values: 0

## input line 487:   x1 += h1
fxch %st(4)
faddp %st(0),%st(1)
## fp stack:  h3 h2 x2#4 x3#4 x0#4 x1#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 6
## live flags values: 0

## input line 488:   x2 += h2
fxch %st(4)
faddp %st(0),%st(3)
## fp stack:  h3 x1#4 x2#4 x3#4 x0#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 5
## live flags values: 0

## input line 489:   x3 += h3
fxch %st(4)
faddp %st(0),%st(1)
## fp stack:  x0#4 x1#4 x2#4 x3#4
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 490: 

## input line 491:   kill h0

## input line 492:   kill h1

## input line 493:   kill h2

## input line 494:   kill h3

## input line 495: 

## input line 496:   x0 += *(float64 *) &poly1305_ppro_hoffset0
fxch %st(3)
## x0#4 += *(float64 *) (&poly1305_ppro_hoffset0 + 0)
## fpstack#0 += *(float64 *) (&poly1305_ppro_hoffset0 + 0)
## %st(0) += *(float64 *) (&poly1305_ppro_hoffset0 + 0)
faddl 0+poly1305_ppro_hoffset0
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 497:   x1 += *(float64 *) &poly1305_ppro_hoffset1
fxch %st(2)
## x1#4 += *(float64 *) (&poly1305_ppro_hoffset1 + 0)
## fpstack#0 += *(float64 *) (&poly1305_ppro_hoffset1 + 0)
## %st(0) += *(float64 *) (&poly1305_ppro_hoffset1 + 0)
faddl 0+poly1305_ppro_hoffset1
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 498:   x2 += *(float64 *) &poly1305_ppro_hoffset2
fxch %st(1)
## x2#4 += *(float64 *) (&poly1305_ppro_hoffset2 + 0)
## fpstack#0 += *(float64 *) (&poly1305_ppro_hoffset2 + 0)
## %st(0) += *(float64 *) (&poly1305_ppro_hoffset2 + 0)
faddl 0+poly1305_ppro_hoffset2
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 499:   x3 += *(float64 *) &poly1305_ppro_hoffset3
fxch %st(3)
## x3#4 += *(float64 *) (&poly1305_ppro_hoffset3 + 0)
## fpstack#0 += *(float64 *) (&poly1305_ppro_hoffset3 + 0)
## %st(0) += *(float64 *) (&poly1305_ppro_hoffset3 + 0)
faddl 0+poly1305_ppro_hoffset3
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 4
## live flags values: 0

## input line 500:   d0 = x0
fxch %st(2)
fstpl 16(%esp)
## fp stack:  x2#4 x3#4 x1#4
## live mem32 values: 7
## live mem64 values: 1
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 3
## live flags values: 0

## input line 501:   d1 = x1
fstpl 24(%esp)
## fp stack:  x2#4 x3#4
## live mem32 values: 7
## live mem64 values: 2
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 2
## live flags values: 0

## input line 502:   d2 = x2
fxch %st(1)
fstpl 32(%esp)
## fp stack:  x3#4
## live mem32 values: 7
## live mem64 values: 3
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 1
## live flags values: 0

## input line 503:   d3 = x3
fstpl 40(%esp)
## fp stack: 
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 504: 

## input line 505: register int32 f0

## input line 506: register int32 f1

## input line 507: register int32 f2

## input line 508: register int32 f3

## input line 509: register int32 f4

## input line 510: register int32 g0

## input line 511: register int32 g1

## input line 512: register int32 g2

## input line 513: register int32 g3

## input line 514: register int32 f

## input line 515: register int32 notf

## input line 516: 

## input line 517:   g0 = top d0
movl 20(%esp),%ecx
## fp stack: 
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 518:   g0 &= 63
## g0 &= 63
## int32#2 &= 63
## %ecx &= 63
and $63,%ecx
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 519:   g1 = top d1
movl 28(%esp),%edx
## fp stack: 
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 520:   g1 &= 63
## g1 &= 63
## int32#3 &= 63
## %edx &= 63
and $63,%edx
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 521:   g2 = top d2
movl 36(%esp),%ebx
## fp stack: 
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 522:   g2 &= 63
## g2 &= 63
## int32#4 &= 63
## %ebx &= 63
and $63,%ebx
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 523:   g3 = top d3
movl 44(%esp),%esi
## fp stack: 
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 524:   g3 &= 63
## g3 &= 63
## int32#5 &= 63
## %esi &= 63
and $63,%esi
## live mem32 values: 7
## live mem64 values: 4
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 525: 

## input line 526:   f1 = bottom d1
movl 24(%esp),%eax
## fp stack: 
## live mem32 values: 7
## live mem64 values: 3
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 527:   carry f1 += g0
## f1 += g0
## int32#1 += int32#2
## %eax += %ecx
add %ecx,%eax
## fp stack: 
## live mem32 values: 7
## live mem64 values: 3
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 528:   store f1
movl %eax,124(%esp)
## fp stack: 
## live mem32 values: 8
## live mem64 values: 3
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 529: 

## input line 530:   f2 = bottom d2
movl 32(%esp),%eax
## fp stack: 
## live mem32 values: 8
## live mem64 values: 2
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 531:   carry f2 += g1 + carry
## carry f2 += g1 + carry
## carry int32#1 += int32#3 + carry
## carry %eax += %edx + carry
adc %edx,%eax
## fp stack: 
## live mem32 values: 8
## live mem64 values: 2
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 532:   store f2
movl %eax,128(%esp)
## fp stack: 
## live mem32 values: 9
## live mem64 values: 2
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 533: 

## input line 534:   f3 = bottom d3
movl 40(%esp),%eax
## fp stack: 
## live mem32 values: 9
## live mem64 values: 1
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 535:   carry f3 += g2 + carry
## carry f3 += g2 + carry
## carry int32#1 += int32#4 + carry
## carry %eax += %ebx + carry
adc %ebx,%eax
## fp stack: 
## live mem32 values: 9
## live mem64 values: 1
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 536:   store f3
movl %eax,132(%esp)
## fp stack: 
## live mem32 values: 10
## live mem64 values: 1
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 537: 

## input line 538:   f4 = 0
## f4 = 0
## int32#1 = 0
## %eax = 0
mov $0,%eax
## fp stack: 
## live mem32 values: 10
## live mem64 values: 1
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 539:   carry f4 += g3 + carry
## carry f4 += g3 + carry
## carry int32#1 += int32#5 + carry
## carry %eax += %esi + carry
adc %esi,%eax
## fp stack: 
## live mem32 values: 10
## live mem64 values: 1
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 540:   store f4
movl %eax,136(%esp)
## fp stack: 
## live mem32 values: 11
## live mem64 values: 1
## live int32 values: 0
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 541: 

## input line 542:   g0 = 5
## g0#2 = 5
## int32#1 = 5
## %eax = 5
mov $5,%eax
## fp stack: 
## live mem32 values: 11
## live mem64 values: 1
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 543:   f0 = bottom d0
movl 16(%esp),%edx
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 544:   carry g0 += f0
## g0#2 += f0
## int32#1 += int32#3
## %eax += %edx
add %edx,%eax
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 545:   store g0
movl %eax,140(%esp)
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 546: 

## input line 547:   g1 = 0
## g1#2 = 0
## int32#1 = 0
## %eax = 0
mov $0,%eax
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 548:   load f1
movl 124(%esp),%ebx
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 549:   carry g1 += f1 + carry
## carry g1#2 += f1#2 + carry
## carry int32#1 += int32#4 + carry
## carry %eax += %ebx + carry
adc %ebx,%eax
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 550:   store g1
movl %eax,124(%esp)
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 551: 

## input line 552:   g2 = 0
## g2#2 = 0
## int32#1 = 0
## %eax = 0
mov $0,%eax
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 553:   load f2
movl 128(%esp),%esi
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 554:   carry g2 += f2 + carry
## carry g2#2 += f2#2 + carry
## carry int32#1 += int32#5 + carry
## carry %eax += %esi + carry
adc %esi,%eax
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 555:   store g2
movl %eax,128(%esp)
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 556: 

## input line 557:   g3 = 0
## g3#2 = 0
## int32#1 = 0
## %eax = 0
mov $0,%eax
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 558:   load f3
movl 132(%esp),%edi
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 559:   carry g3 += f3 + carry
## carry g3#2 += f3#2 + carry
## carry int32#1 += int32#6 + carry
## carry %eax += %edi + carry
adc %edi,%eax
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 560:   store g3
movl %eax,132(%esp)
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 561: 

## input line 562:   f = "-4"
## f = -4
## int32#2 = -4
## %ecx = -4
mov $-4,%ecx
## fp stack: 
## live mem32 values: 12
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 563:   load f4
movl 136(%esp),%eax
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 1

## input line 564:   carry f += f4 + carry
## carry f += f4#2 + carry
## carry int32#2 += int32#1 + carry
## carry %ecx += %eax + carry
adc %eax,%ecx
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 565: 

## input line 566:   signed f >>= 16
## signed f >>= 16
## signed int32#2 >>= 16
## signed %ecx >>= 16
sar $16,%ecx
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 567: 

## input line 568:   notf = f
## notf = f
## int32#7 = int32#2
## %ebp = %ecx
mov %ecx,%ebp
## fp stack: 
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 569:   notf ^= "-1"
## notf ^= -1
## int32#7 ^= -1
## %ebp ^= -1
xor $-1,%ebp
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 570:   f0 &= f
## f0 &= f
## int32#3 &= int32#2
## %edx &= %ecx
and %ecx,%edx
## live mem32 values: 11
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 571:   load g0
movl 140(%esp),%eax
## fp stack: 
## live mem32 values: 10
## live mem64 values: 0
## live int32 values: 7
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 572:   g0 &= notf
## g0#3 &= notf
## int32#1 &= int32#7
## %eax &= %ebp
and %ebp,%eax
## live mem32 values: 10
## live mem64 values: 0
## live int32 values: 7
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 573:   f0 |= g0
## f0 |= g0#3
## int32#3 |= int32#1
## %edx |= %eax
or %eax,%edx
## fp stack: 
## live mem32 values: 10
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 574:   f1 &= f
## f1#2 &= f
## int32#4 &= int32#2
## %ebx &= %ecx
and %ecx,%ebx
## live mem32 values: 10
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 575:   load g1
movl 124(%esp),%eax
## fp stack: 
## live mem32 values: 9
## live mem64 values: 0
## live int32 values: 7
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 576:   g1 &= notf
## g1#3 &= notf
## int32#1 &= int32#7
## %eax &= %ebp
and %ebp,%eax
## live mem32 values: 9
## live mem64 values: 0
## live int32 values: 7
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 577:   f1 |= g1
## f1#2 |= g1#3
## int32#4 |= int32#1
## %ebx |= %eax
or %eax,%ebx
## fp stack: 
## live mem32 values: 9
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 578:   f2 &= f
## f2#2 &= f
## int32#5 &= int32#2
## %esi &= %ecx
and %ecx,%esi
## live mem32 values: 9
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 579:   load g2
movl 128(%esp),%eax
## fp stack: 
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 7
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 580:   g2 &= notf
## g2#3 &= notf
## int32#1 &= int32#7
## %eax &= %ebp
and %ebp,%eax
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 7
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 581:   f2 |= g2
## f2#2 |= g2#3
## int32#5 |= int32#1
## %esi |= %eax
or %eax,%esi
## fp stack: 
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 582:   f3 &= f
## f3#2 &= f
## int32#6 &= int32#2
## %edi &= %ecx
and %ecx,%edi
## fp stack: 
## live mem32 values: 8
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 583:   load g3
movl 132(%esp),%eax
## fp stack: 
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 584:   g3 &= notf
## g3#3 &= notf
## int32#1 &= int32#7
## %eax &= %ebp
and %ebp,%eax
## fp stack: 
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 585:   f3 |= g3
## f3#2 |= g3#3
## int32#6 |= int32#1
## %edi |= %eax
or %eax,%edi
## fp stack: 
## live mem32 values: 7
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 586: 

## input line 587:   load callerint stackoffset
movl 120(%esp),%eax
## fp stack: 
## live mem32 values: 6
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 588:   load s
movl 12(%esp,%eax),%ecx
## fp stack: 
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 589: 

## input line 590:   carry f0 += *(uint32 *) (s + 0)
## f0 += *(uint32 *) (s + 0)
## int32#3 += *(uint32 *) (int32#2 + 0)
## %edx += *(uint32 *) (%ecx + 0)
addl 0(%ecx),%edx
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 591:   carry f1 += *(uint32 *) (s + 4) + carry
## f1#2 += *(uint32 *) (s + 4) + carry
## int32#4 += *(uint32 *) (int32#2 + 4) + carry
## %ebx += *(uint32 *) (%ecx + 4) + carry
adcl 4(%ecx),%ebx
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 592:   carry f2 += *(uint32 *) (s + 8) + carry
## f2#2 += *(uint32 *) (s + 8) + carry
## int32#5 += *(uint32 *) (int32#2 + 8) + carry
## %esi += *(uint32 *) (%ecx + 8) + carry
adcl 8(%ecx),%esi
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 593:   carry f3 += *(uint32 *) (s + 12) + carry
## f3#2 += *(uint32 *) (s + 12) + carry
## int32#6 += *(uint32 *) (int32#2 + 12) + carry
## %edi += *(uint32 *) (%ecx + 12) + carry
adcl 12(%ecx),%edi
## fp stack: 
## live mem32 values: 5
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 594: 

## input line 595:   load out
movl 4(%esp,%eax),%ecx
## fp stack: 
## live mem32 values: 4
## live mem64 values: 0
## live int32 values: 6
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 596: 

## input line 597:   *(uint32 *) (out + 0) = f0
## *(uint32 *) (out + 0) = f0
## *(uint32 *) (int32#2 + 0) = int32#3
## *(uint32 *) (%ecx + 0) = %edx
movl %edx,0(%ecx)
## fp stack: 
## live mem32 values: 4
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 598:   *(uint32 *) (out + 4) = f1
## *(uint32 *) (out + 4) = f1#2
## *(uint32 *) (int32#2 + 4) = int32#4
## *(uint32 *) (%ecx + 4) = %ebx
movl %ebx,4(%ecx)
## fp stack: 
## live mem32 values: 4
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 599:   *(uint32 *) (out + 8) = f2
## *(uint32 *) (out + 8) = f2#2
## *(uint32 *) (int32#2 + 8) = int32#5
## *(uint32 *) (%ecx + 8) = %esi
movl %esi,8(%ecx)
## fp stack: 
## live mem32 values: 4
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 600:   *(uint32 *) (out + 12) = f3
## *(uint32 *) (out + 12) = f3#2
## *(uint32 *) (int32#2 + 12) = int32#6
## *(uint32 *) (%ecx + 12) = %edi
movl %edi,12(%ecx)
## fp stack: 
## live mem32 values: 4
## live mem64 values: 0
## live int32 values: 1
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 601: 

## input line 602:   load callerint ebx
movl 104(%esp),%ebx
## fp stack: 
## live mem32 values: 3
## live mem64 values: 0
## live int32 values: 2
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 603:   load callerint esi
movl 108(%esp),%esi
## fp stack: 
## live mem32 values: 2
## live mem64 values: 0
## live int32 values: 3
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 604:   load callerint edi
movl 112(%esp),%edi
## fp stack: 
## live mem32 values: 1
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 605:   load callerint ebp
movl 116(%esp),%ebp
## fp stack: 
## live mem32 values: 0
## live mem64 values: 0
## live int32 values: 5
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0

## input line 606: 

## input line 607: leave
add %eax,%esp
ret
## fp stack: 
## live mem32 values: 0
## live mem64 values: 0
## live int32 values: 4
## live mem128 values: 0
## live float80 values: 0
## live flags values: 0
