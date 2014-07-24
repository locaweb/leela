/*
poly1305aes_athlon_verify.c version 20050218
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_athlon.h"
#include "poly1305_athlon.h"
#include "aes_athlon.h"

int poly1305aes_athlon_verify(const unsigned char a[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  unsigned char valid[16];
  aes_athlon(aeskn,k,n);
  poly1305_athlon(valid,r,aeskn,m,l);
  return poly1305aes_athlon_isequal(a,valid);
}
