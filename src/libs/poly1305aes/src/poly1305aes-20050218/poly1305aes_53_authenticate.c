/*
poly1305aes_53_authenticate.c version 20050203
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_53.h"
#include "poly1305_53.h"
#include "aes_big.h"

void poly1305aes_53_authenticate(unsigned char out[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  aes_big(aeskn,k,n);
  poly1305_53(out,r,aeskn,m,l);
}
