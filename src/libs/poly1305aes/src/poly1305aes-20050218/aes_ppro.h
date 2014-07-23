/*
aes_ppro.h version 20050213
D. J. Bernstein
Public domain.
*/

#ifndef AES_PPRO_H
#define AES_PPRO_H

extern void aes_ppro(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]);

#ifndef aes_implementation
#define aes_implementation "aes_ppro"
#define aes aes_ppro
#endif

#endif
