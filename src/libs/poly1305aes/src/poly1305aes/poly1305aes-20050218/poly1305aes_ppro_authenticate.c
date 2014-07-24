/*
poly1305aes_ppro_authenticate.c version 20050213
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_ppro.h"
#include "poly1305_ppro.h"
#include "aes_ppro.h"

void poly1305aes_ppro_authenticate(unsigned char out[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  aes_ppro(aeskn,k,n);
  poly1305_ppro(out,r,aeskn,m,l);
}
