/*
aes_sparc.h version 20050203
D. J. Bernstein
Public domain.
*/

#ifndef AES_SPARC_H
#define AES_SPARC_H

extern void aes_sparc(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]);

#ifndef aes_implementation
#define aes_implementation "aes_sparc"
#define aes aes_sparc
#endif

extern const unsigned int aes_sparc_constants[1034];

#endif
