/*
poly1305aes_macos_authenticate.c version 20050207
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_macos.h"
#include "poly1305_macos.h"
#include "aes_macos.h"

void poly1305aes_macos_authenticate(unsigned char out[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  aes_macos(aeskn,k,n);
  poly1305_macos(out,r,aeskn,m,l);
}
