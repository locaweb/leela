/*
poly1305_macos.h version 20050207
D. J. Bernstein
Public domain.
*/

#ifndef POLY1305_MACOS_H
#define POLY1305_MACOS_H

extern void poly1305_macos(unsigned char out[16],
  const unsigned char r[16],
  const unsigned char s[16],
  const unsigned char m[],unsigned int l);

#ifndef poly1305_implementation
#define poly1305_implementation "poly1305_macos"
#define poly1305 poly1305_macos
#endif

#endif
