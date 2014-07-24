/*
poly1305_ppro.h version 20050213
D. J. Bernstein
Public domain.
*/

#ifndef POLY1305_PPRO_H
#define POLY1305_PPRO_H

extern void poly1305_ppro(unsigned char out[16],
  const unsigned char r[16],
  const unsigned char s[16],
  const unsigned char m[],unsigned int l);

#ifndef poly1305_implementation
#define poly1305_implementation "poly1305_ppro"
#define poly1305 poly1305_ppro
#endif

#endif
