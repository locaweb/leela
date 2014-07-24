/*
test-poly1305aes version 20050131
D. J. Bernstein
Public domain.
*/

#include <stdio.h>
#include "poly1305aes.h"

#define MAXLEN 1000
unsigned char out[16];
unsigned char kr[32];
unsigned char n[16];
unsigned char m[MAXLEN];

main()
{
  int loop;
  int len;
  int i;
  int x;
  int y;

  for (loop = 0;loop < 1000000;++loop) {
    len = 0;
    for (;;) {
      poly1305aes_authenticate(out,kr,n,m,len);
      for (i = 0;i < 16;++i) printf("%02x",(unsigned int) out[i]);
      printf("\n");
      if (!poly1305aes_verify(out,kr,n,m,len)) {
        printf("poly1305aes_verify failed\n");
        return 1;
      }
      x = random() & 15;
      y = 1 + (random() % 255);
      out[x] += y;
      if (poly1305aes_verify(out,kr,n,m,len)) {
        printf("poly1305aes_verify succeeded on bad input\n");
        return 1;
      }
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
  return 0;
}
