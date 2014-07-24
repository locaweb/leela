/*
aes_big.c version 20050203
D. J. Bernstein
Public domain.
*/

#include "aes_big.h"

#define uchar unsigned char
#define int32 int
#define uint32 unsigned int

void aes_big(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]
)
{
  register int32 loop4;
  register char *table0;
  register char *table1;
  register char *table2;
  register char *table3;
  register uint32 x0;
  register uint32 x1;
  register uint32 x2;
  register uint32 x3;
  register uint32 y0;
  register uint32 y1;
  register uint32 y2;
  register uint32 y3;
  register uint32 byte0;
  register uint32 byte1;
  register uint32 byte2;
  register uint32 byte3;
  register uint32 e;
  register uint32 p00;
  register uint32 p01;
  register uint32 p02;
  register uint32 p03;
  register uint32 z0;
  register uint32 z1;
  register uint32 z2;
  register uint32 z3;
  register uint32 p10;
  register uint32 p11;
  register uint32 p12;
  register uint32 p13;
  register uint32 p20;
  register uint32 p21;
  register uint32 p22;
  register uint32 p23;
  register uint32 p30;
  register uint32 p31;
  register uint32 p32;
  register uint32 p33;
  register uint32 q0;
  register uint32 q1;
  register uint32 q2;
  register uint32 q3;
  register uint32 k00;
  register uint32 k01;
  register uint32 k02;
  register uint32 k03;
  register uint32 k10;
  register uint32 k11;
  register uint32 k12;
  register uint32 k13;
  register uint32 k20;
  register uint32 k21;
  register uint32 k22;
  register uint32 k23;
  register uint32 k30;
  register uint32 k31;
  register uint32 k32;
  register uint32 k33;
  register uint32 n00;
  register uint32 n01;
  register uint32 n02;
  register uint32 n03;
  register uint32 n10;
  register uint32 n11;
  register uint32 n12;
  register uint32 n13;
  register uint32 n20;
  register uint32 n21;
  register uint32 n22;
  register uint32 n23;
  register uint32 n30;
  register uint32 n31;
  register uint32 n32;
  register uint32 n33;

  table0 = (char *) &aes_big_constants[10];
  table1 = table0 + 8;
  table2 = table0 + 4;
  table3 = table0 + 12;
  
  byte0 = 0xff;
  byte1 = 0xff00;
  byte2 = 0xff0000;
  byte3 = 0xff000000;
  
  loop4 = -36;
  
  k30 = *(uchar *) (k + 15);
  k31 = *(uchar *) (k + 14);
  k32 = *(uchar *) (k + 13);
  k33 = *(uchar *) (k + 12);
  k31 <<= 8;
  k32 <<= 16;
  k33 <<= 24;
  x3 = k30 ^ k31;
  x3 ^= k32;
  x3 ^= k33;
  
  k00 = *(uchar *) (k + 3);
  k01 = *(uchar *) (k + 2);
  k02 = *(uchar *) (k + 1);
  k03 = *(uchar *) (k + 0);
  k01 <<= 8;
  k02 <<= 16;
  k03 <<= 24;
  x0 = k00 ^ k01;
  x0 ^= k02;
  x0 ^= k03;
  
  k10 = *(uchar *) (k + 7);
  k11 = *(uchar *) (k + 6);
  k12 = *(uchar *) (k + 5);
  k13 = *(uchar *) (k + 4);
  k11 <<= 8;
  k12 <<= 16;
  k13 <<= 24;
  x1 = k10 ^ k11;
  x1 ^= k12;
  x1 ^= k13;
  
  k20 = *(uchar *) (k + 11);
  k21 = *(uchar *) (k + 10);
  k22 = *(uchar *) (k + 9);
  k23 = *(uchar *) (k + 8);
  k21 <<= 8;
  k22 <<= 16;
  k23 <<= 24;
  x2 = k20 ^ k21;
  x2 ^= k22;
  x2 ^= k23;
  
  q0 = x3 >> 12;
  q1 = x3 >> 4;
  q2 = x3 << 4;
  q3 = x3 >> 20;
  q0 &= 4080;
  q1 &= 4080;
  q2 &= 4080;
  q3 &= 4080;
  q0 = *(uint32 *) (table2 + q0);
  q1 = *(uint32 *) (table3 + q1);
  q2 = *(uint32 *) (table0 + q2);
  q3 = *(uint32 *) (table1 + q3);
  q0 &= byte3;
  q1 &= byte2;
  q2 &= byte1;
  q3 &= byte0;
  e = q0 ^ aes_big_constants[0];
  e ^= q1;
  e ^= q2;
  
  n00 = *(uchar *) (n + 3);
  n01 = *(uchar *) (n + 2);
  n02 = *(uchar *) (n + 1);
  n03 = *(uchar *) (n + 0);
  n01 <<= 8;
  n02 <<= 16;
  n03 <<= 24;
  y0 = x0 ^ n00;
  y0 ^= n01;
  y0 ^= n02;
  y0 ^= n03;
  
  n10 = *(uchar *) (n + 7);
  n11 = *(uchar *) (n + 6);
  n12 = *(uchar *) (n + 5);
  n13 = *(uchar *) (n + 4);
  n11 <<= 8;
  n12 <<= 16;
  n13 <<= 24;
  y1 = x1 ^ n10;
  y1 ^= n11;
  y1 ^= n12;
  y1 ^= n13;
  
  n20 = *(uchar *) (n + 11);
  n21 = *(uchar *) (n + 10);
  n22 = *(uchar *) (n + 9);
  n23 = *(uchar *) (n + 8);
  n21 <<= 8;
  n22 <<= 16;
  n23 <<= 24;
  y2 = x2 ^ n20;
  y2 ^= n21;
  y2 ^= n22;
  y2 ^= n23;
  
  n30 = *(uchar *) (n + 15);
  n31 = *(uchar *) (n + 14);
  n32 = *(uchar *) (n + 13);
  n33 = *(uchar *) (n + 12);
  n31 <<= 8;
  n32 <<= 16;
  n33 <<= 24;
  y3 = x3 ^ n30;
  y3 ^= n31;
  y3 ^= n32;
  y3 ^= n33;
  
  do {
    e ^= q3;
    x0 ^= e;
    x1 ^= x0;
    x2 ^= x1;
    x3 ^= x2;

    p00 = y0 >> 20;
    p01 = y0 >> 12;
    p02 = y0 >> 4;
    p03 = y0 << 4;
    p00 &= 4080;
    p01 &= 4080;
    p02 &= 4080;
    p03 &= 4080;
    p00 = *(uint32 *) (table0 + p00);
    p01 = *(uint32 *) (table1 + p01);
    p02 = *(uint32 *) (table2 + p02);
    p03 = *(uint32 *) (table3 + p03);
    z0 = x0 ^ p00;
    z3 = x3 ^ p01;
    z2 = x2 ^ p02;
    z1 = x1 ^ p03;
  
    p10 = y1 >> 20;
    p11 = y1 >> 12;
    p12 = y1 >> 4;
    p13 = y1 << 4;
    p10 &= 4080;
    p11 &= 4080;
    p12 &= 4080;
    p13 &= 4080;
    p10 = *(uint32 *) (table0 + p10);
    p11 = *(uint32 *) (table1 + p11);
    p12 = *(uint32 *) (table2 + p12);
    p13 = *(uint32 *) (table3 + p13);
    z1 ^= p10;
    z0 ^= p11;
    z3 ^= p12;
    z2 ^= p13;
  
    p20 = y2 >> 20;
    p21 = y2 >> 12;
    p22 = y2 >> 4;
    p23 = y2 << 4;
    p20 &= 4080;
    p21 &= 4080;
    p22 &= 4080;
    p23 &= 4080;
    p20 = *(uint32 *) (table0 + p20);
    p21 = *(uint32 *) (table1 + p21);
    p22 = *(uint32 *) (table2 + p22);
    p23 = *(uint32 *) (table3 + p23);
    z2 ^= p20;
    z1 ^= p21;
    z0 ^= p22;
    z3 ^= p23;

    p31 = y3 >> 12;
    p32 = y3 >> 4;
    p33 = y3 << 4;
    p30 = y3 >> 20;
    p31 &= 4080;
    p32 &= 4080;
    p33 &= 4080;
    p30 &= 4080;
    p31 = *(uint32 *) (table1 + p31);
    p32 = *(uint32 *) (table2 + p32);
    p33 = *(uint32 *) (table3 + p33);
    p30 = *(uint32 *) (table0 + p30);
    y2 = z2 ^ p31;
    y1 = z1 ^ p32;
    y0 = z0 ^ p33;
    y3 = z3 ^ p30;

    e = *(uint32 *) (table0 + loop4);
  
    q0 = x3 >> 12;
    q1 = x3 >> 4;
    q2 = x3 << 4;
    q3 = x3 >> 20;
    q0 &= 4080;
    q1 &= 4080;
    q2 &= 4080;
    q3 &= 4080;
    q0 = *(uint32 *) (table2 + q0);
    q1 = *(uint32 *) (table3 + q1);
    q2 = *(uint32 *) (table0 + q2);
    q3 = *(uint32 *) (table1 + q3);
    q0 &= byte3;
    q1 &= byte2;
    q2 &= byte1;
    q3 &= byte0;
    e ^= q0;
    e ^= q1;
    e ^= q2;

    loop4 += 4;
  } while (loop4);
  
  e = e ^ q3;
  x0 ^= e;
  x1 ^= x0;
  x2 ^= x1;
  x3 ^= x2;
  
  p00 = y0 >> 20;
  p01 = y0 >> 12;
  p02 = y0 >> 4;
  p03 = y0 << 4;
  p00 &= 4080;
  p01 &= 4080;
  p02 &= 4080;
  p03 &= 4080;
  p00 = *(uint32 *) (table2 + p00);
  p01 = *(uint32 *) (table3 + p01);
  p02 = *(uint32 *) (table0 + p02);
  p03 = *(uint32 *) (table1 + p03);
  p00 &= byte3;
  p01 &= byte2;
  p02 &= byte1;
  p03 &= byte0;
  z0 = x0 ^ p00;
  z3 = x3 ^ p01;
  z2 = x2 ^ p02;
  z1 = x1 ^ p03;
  
  p10 = y1 >> 20;
  p11 = y1 >> 12;
  p12 = y1 >> 4;
  p13 = y1 << 4;
  p10 &= 4080;
  p11 &= 4080;
  p12 &= 4080;
  p13 &= 4080;
  p10 = *(uint32 *) (table2 + p10);
  p11 = *(uint32 *) (table3 + p11);
  p12 = *(uint32 *) (table0 + p12);
  p13 = *(uint32 *) (table1 + p13);
  p10 &= byte3;
  p11 &= byte2;
  p12 &= byte1;
  p13 &= byte0;
  z1 ^= p10;
  z0 ^= p11;
  z3 ^= p12;
  z2 ^= p13;
  
  p20 = y2 >> 20;
  p21 = y2 >> 12;
  p22 = y2 >> 4;
  p23 = y2 << 4;
  p20 &= 4080;
  p21 &= 4080;
  p22 &= 4080;
  p23 &= 4080;
  p20 = *(uint32 *) (table2 + p20);
  p21 = *(uint32 *) (table3 + p21);
  p22 = *(uint32 *) (table0 + p22);
  p23 = *(uint32 *) (table1 + p23);
  p20 &= byte3;
  p21 &= byte2;
  p22 &= byte1;
  p23 &= byte0;
  z3 ^= p23;
  z2 ^= p20;
  z1 ^= p21;
  z0 ^= p22;
  
  p30 = y3 >> 20;
  p31 = y3 >> 12;
  p32 = y3 >> 4;
  p33 = y3 << 4;
  p30 &= 4080;
  p31 &= 4080;
  p32 &= 4080;
  p33 &= 4080;
  p30 = *(uint32 *) (table2 + p30);
  p31 = *(uint32 *) (table3 + p31);
  p32 = *(uint32 *) (table0 + p32);
  p33 = *(uint32 *) (table1 + p33);
  p30 &= byte3;
  p31 &= byte2;
  p32 &= byte1;
  p33 &= byte0;
  y3 = z3 ^ p30;
  y2 = z2 ^ p31;
  y1 = z1 ^ p32;
  y0 = z0 ^ p33;
  
  *(uchar *) (out + 3) = y0;
  y0 >>= 8;
  *(uchar *) (out + 2) = y0;
  y0 >>= 8;
  *(uchar *) (out + 1) = y0;
  y0 >>= 8;
  *(uchar *) (out + 0) = y0;
  
  *(uchar *) (out + 7) = y1;
  y1 >>= 8;
  *(uchar *) (out + 6) = y1;
  y1 >>= 8;
  *(uchar *) (out + 5) = y1;
  y1 >>= 8;
  *(uchar *) (out + 4) = y1;
  
  *(uchar *) (out + 11) = y2;
  y2 >>= 8;
  *(uchar *) (out + 10) = y2;
  y2 >>= 8;
  *(uchar *) (out + 9) = y2;
  y2 >>= 8;
  *(uchar *) (out + 8) = y2;
  
  *(uchar *) (out + 15) = y3;
  y3 >>= 8;
  *(uchar *) (out + 14) = y3;
  y3 >>= 8;
  *(uchar *) (out + 13) = y3;
  y3 >>= 8;
  *(uchar *) (out + 12) = y3;
}
