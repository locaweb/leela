/*
poly1305aes_53_isequal.c version 20050203
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_53.h"

#define uchar unsigned char
#define uint32 unsigned int

int poly1305aes_53_isequal(const unsigned char x[16],
  const unsigned char y[16])
{
  register uint32 d;
  register uint32 x0;
  register uint32 x1;
  register uint32 x2;
  register uint32 x3;
  register uint32 x4;
  register uint32 x5;
  register uint32 x6;
  register uint32 x7;
  register uint32 x8;
  register uint32 x9;
  register uint32 x10;
  register uint32 x11;
  register uint32 x12;
  register uint32 x13;
  register uint32 x14;
  register uint32 x15;
  register uint32 y0;
  register uint32 y1;
  register uint32 y2;
  register uint32 y3;
  register uint32 y4;
  register uint32 y5;
  register uint32 y6;
  register uint32 y7;
  register uint32 y8;
  register uint32 y9;
  register uint32 y10;
  register uint32 y11;
  register uint32 y12;
  register uint32 y13;
  register uint32 y14;
  register uint32 y15;

  x0 = *(uchar *) (x + 0);
  y0 = *(uchar *) (y + 0);
  x1 = *(uchar *) (x + 1);
  y1 = *(uchar *) (y + 1);
  x2 = *(uchar *) (x + 2);
  y2 = *(uchar *) (y + 2);
  d = y0 ^ x0;
  x3 = *(uchar *) (x + 3);
  y1 ^= x1;
  y3 = *(uchar *) (y + 3);
  d |= y1;
  x4 = *(uchar *) (x + 4);
  y2 ^= x2;
  y4 = *(uchar *) (y + 4);
  d |= y2;
  x5 = *(uchar *) (x + 5);
  y3 ^= x3;
  y5 = *(uchar *) (y + 5);
  d |= y3;
  x6 = *(uchar *) (x + 6);
  y4 ^= x4;
  y6 = *(uchar *) (y + 6);
  d |= y4;
  x7 = *(uchar *) (x + 7);
  y5 ^= x5;
  y7 = *(uchar *) (y + 7);
  d |= y5;
  x8 = *(uchar *) (x + 8);
  y6 ^= x6;
  y8 = *(uchar *) (y + 8);
  d |= y6;
  x9 = *(uchar *) (x + 9);
  y7 ^= x7;
  y9 = *(uchar *) (y + 9);
  d |= y7;
  x10 = *(uchar *) (x + 10);
  y8 ^= x8;
  y10 = *(uchar *) (y + 10);
  d |= y8;
  x11 = *(uchar *) (x + 11);
  y9 ^= x9;
  y11 = *(uchar *) (y + 11);
  d |= y9;
  x12 = *(uchar *) (x + 12);
  y10 ^= x10;
  y12 = *(uchar *) (y + 12);
  d |= y10;
  x13 = *(uchar *) (x + 13);
  y11 ^= x11;
  y13 = *(uchar *) (y + 13);
  d |= y11;
  x14 = *(uchar *) (x + 14);
  y12 ^= x12;
  y14 = *(uchar *) (y + 14);
  d |= y12;
  x15 = *(uchar *) (x + 15);
  y13 ^= x13;
  y15 = *(uchar *) (y + 15);
  d |= y13;
  y14 ^= x14;
  d |= y14;
  y15 ^= x15;
  d |= y15;
  d -= 1;
  d >>= 8;

  return d;
}
