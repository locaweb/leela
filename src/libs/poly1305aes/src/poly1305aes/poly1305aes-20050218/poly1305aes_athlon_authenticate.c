/*
poly1305aes_athlon_authenticate.c version 20050218
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_athlon.h"
#include "poly1305_athlon.h"
#include "aes_athlon.h"

void poly1305aes_athlon_authenticate(unsigned char out[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  aes_athlon(aeskn,k,n);
  poly1305_athlon(out,r,aeskn,m,l);
}
