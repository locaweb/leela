/*
aes_athlon.h version 20050218
D. J. Bernstein
Public domain.
*/

#ifndef AES_ATHLON_H
#define AES_ATHLON_H

extern void aes_athlon(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]);

#ifndef aes_implementation
#define aes_implementation "aes_athlon"
#define aes aes_athlon
#endif

#endif
