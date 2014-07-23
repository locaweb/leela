/*
poly1305aes_53_verify.c version 20050203
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_53.h"
#include "poly1305_53.h"
#include "aes_big.h"

int poly1305aes_53_verify(const unsigned char a[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  unsigned char valid[16];
  aes_big(aeskn,k,n);
  poly1305_53(valid,r,aeskn,m,l);
  return poly1305aes_53_isequal(a,valid);
}
