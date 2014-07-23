/*
poly1305_sparc.h version 20050201
D. J. Bernstein
Public domain.
*/

#ifndef POLY1305_SPARC_H
#define POLY1305_SPARC_H

extern void poly1305_sparc(unsigned char out[16],
  const unsigned char r[16],
  const unsigned char s[16],
  const unsigned char m[],unsigned int l);

#ifndef poly1305_implementation
#define poly1305_implementation "poly1305_sparc"
#define poly1305 poly1305_sparc
#endif

#endif
