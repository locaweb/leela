/*
poly1305aes_sparc_authenticate.c version 20050131
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_sparc.h"
#include "poly1305_sparc.h"
#include "aes_sparc.h"

void poly1305aes_sparc_authenticate(unsigned char out[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  poly1305aes_sparc_fsr();
  aes_sparc(aeskn,k,n);
  poly1305_sparc(out,r,aeskn,m,l);
}
