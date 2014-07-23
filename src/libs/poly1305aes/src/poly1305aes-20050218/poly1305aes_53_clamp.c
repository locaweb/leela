/*
poly1305aes_53_clamp.c version 20050207
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_53.h"

void poly1305aes_53_clamp(unsigned char kr[32])
{
  unsigned int r3;
  unsigned int r7;
  unsigned int r11;
  unsigned int r15;
  unsigned int r4;
  unsigned int r8;
  unsigned int r12;
#define r (kr + 16)
  r3 = r[3];
  r7 = r[7];
  r11 = r[11];
  r15 = r[15];
  r4 = r[4];
  r8 = r[8];
  r12 = r[12];
  r3 &= 15;
  r7 &= 15;
  r11 &= 15;
  r15 &= 15;
  r4 &= 252;
  r8 &= 252;
  r12 &= 252;
  r[3] = r3;
  r[7] = r7;
  r[11] = r11;
  r[15] = r15;
  r[4] = r4;
  r[8] = r8;
  r[12] = r12;
}
