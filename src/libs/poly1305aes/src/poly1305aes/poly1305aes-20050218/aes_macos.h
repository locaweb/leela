/*
aes_macos.h version 20050207
D. J. Bernstein
Public domain.
*/

#ifndef AES_MACOS_H
#define AES_MACOS_H

extern void aes_macos(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]);

#ifndef aes_implementation
#define aes_implementation "aes_macos"
#define aes aes_macos
#endif

#endif
