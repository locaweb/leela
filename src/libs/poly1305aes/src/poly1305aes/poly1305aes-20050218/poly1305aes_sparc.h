/*
poly1305aes_sparc.h version 20050203
D. J. Bernstein
Public domain.
*/

#ifndef POLY1305AES_SPARC_H
#define POLY1305AES_SPARC_H

extern void poly1305aes_sparc_clamp(unsigned char kr[32]);

extern void poly1305aes_sparc_authenticate(unsigned char out[16],
  const unsigned char kr[32],
  const unsigned char n[16],
  const unsigned char m[],unsigned int l);

extern int poly1305aes_sparc_verify(const unsigned char a[16],
  const unsigned char kr[32],
  const unsigned char n[16],
  const unsigned char m[],unsigned int l);

#ifndef poly1305aes_implementation
#define poly1305aes_implementation "poly1305aes_sparc"
#define poly1305aes_clamp poly1305aes_sparc_clamp
#define poly1305aes_authenticate poly1305aes_sparc_authenticate
#define poly1305aes_verify poly1305aes_sparc_verify
#endif

extern int poly1305aes_sparc_isequal(const unsigned char x[16],
  const unsigned char y[16]);

#endif
