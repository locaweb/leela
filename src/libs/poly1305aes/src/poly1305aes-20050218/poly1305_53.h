/*
poly1305_53.h version 20050203
D. J. Bernstein
Public domain.
*/

#ifndef POLY1305_53_H
#define POLY1305_53_H

extern void poly1305_53(unsigned char out[16],
  const unsigned char r[16],
  const unsigned char s[16],
  const unsigned char m[],unsigned int l);

#ifndef poly1305_implementation
#define poly1305_implementation "poly1305_53"
#define poly1305 poly1305_53
#endif

extern const double poly1305_53_constants[];

#endif
