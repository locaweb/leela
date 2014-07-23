/*
poly1305aes_macos_verify.c version 20050207
D. J. Bernstein
Public domain.
*/

#include "poly1305aes_macos.h"
#include "poly1305_macos.h"
#include "aes_macos.h"

int poly1305aes_macos_verify(const unsigned char a[16],
  const unsigned char kr[32],
#define k (kr + 0)
#define r (kr + 16)
  const unsigned char n[16],
  const unsigned char m[],unsigned int l)
{
  unsigned char aeskn[16];
  unsigned char valid[16];
  aes_macos(aeskn,k,n);
  poly1305_macos(valid,r,aeskn,m,l);
  return poly1305aes_macos_isequal(a,valid);
}
