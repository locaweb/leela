/*
poly1305aes_ppro_verify.c version 20050213
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_ppro.h"
#include "poly1305_ppro.h"
#include "aes_ppro.h"

int poly1305aes_ppro_verify(const unsigned char a[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  unsigned char valid[16];
  aes_ppro(aeskn,k,n);
  poly1305_ppro(valid,r,aeskn,m,l);
  return poly1305aes_ppro_isequal(a,valid);
}
