/*
poly1305_aix.h version 20050205
D. J. Bernstein
Public domain.
*/

#ifndef POLY1305_AIX_H
#define POLY1305_AIX_H

extern void poly1305_aix(unsigned char out[16],
  const unsigned char r[16],
  const unsigned char s[16],
  const unsigned char m[],unsigned int l);

#ifndef poly1305_implementation
#define poly1305_implementation "poly1305_aix"
#define poly1305 poly1305_aix
#endif

#endif
