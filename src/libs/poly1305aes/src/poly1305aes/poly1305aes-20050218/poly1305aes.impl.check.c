/*
poly1305aes.impl.check.c version 20050218
D. J. Bernstein
Public domain.
*/

#include "poly1305aes.impl.check.h"

#define MAXLEN 100
unsigned char out[16];
unsigned char kr[32];
unsigned char n[16];
unsigned char m[MAXLEN];

unsigned char exp[16] = {
0xf7,0xe7,0x71,0xd9,0xb6,0x09,0x67,0xcc,0x6a,0xaa,0x9a,0x04,0x1b,0xe5,0x53,0x65
};

main()
{
  int loop;
  int len;
  int i;
  int x;
  int y;

  for (loop = 0;loop < 10;++loop) {
    len = 0;
    for (;;) {
      poly1305aes_authenticate(out,kr,n,m,len);
      if (!poly1305aes_verify(out,kr,n,m,len)) return 1;
      x = random() & 15;
      y = 1 + (random() % 255);
      out[x] += y;
      if (poly1305aes_verify(out,kr,n,m,len)) return 1;
      out[x] -= y;
      if (len >= MAXLEN) break;
      n[0] ^= loop;
      for (i = 0;i < 16;++i) n[i] ^= out[i];
      if (len % 2) for (i = 0;i < 16;++i) kr[i] ^= out[i];
      if (len % 3) for (i = 0;i < 16;++i) kr[i + 16] ^= out[i];
      poly1305aes_clamp(kr);
      m[len++] ^= out[0];
    }
  }

  for (i = 0;i < 16;++i) if (out[i] != exp[i]) return 1;
  return 0;
}
