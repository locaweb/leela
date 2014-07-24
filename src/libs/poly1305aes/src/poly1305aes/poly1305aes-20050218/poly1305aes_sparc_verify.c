/*
poly1305aes_sparc_verify.c version 20050131
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_sparc.h"
#include "poly1305_sparc.h"
#include "aes_sparc.h"

int poly1305aes_sparc_verify(const unsigned char a[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  unsigned char valid[16];
  poly1305aes_sparc_fsr();
  aes_sparc(aeskn,k,n);
  poly1305_sparc(valid,r,aeskn,m,l);
  return poly1305aes_sparc_isequal(a,valid);
}
