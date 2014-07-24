/*
poly1305aes_aix_verify.c version 20050205
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_aix.h"
#include "poly1305_aix.h"
#include "aes_aix.h"

int poly1305aes_aix_verify(const unsigned char a[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  unsigned char valid[16];
  aes_aix(aeskn,k,n);
  poly1305_aix(valid,r,aeskn,m,l);
  return poly1305aes_aix_isequal(a,valid);
}
