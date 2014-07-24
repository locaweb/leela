/*
aes_big.h version 20050203
D. J. Bernstein
Public domain.
*/

#ifndef AES_BIG_H
#define AES_BIG_H

extern void aes_big(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]);

#ifndef aes_implementation
#define aes_implementation "aes_big"
#define aes aes_big
#endif

extern const unsigned int aes_big_constants[1034];

#endif
