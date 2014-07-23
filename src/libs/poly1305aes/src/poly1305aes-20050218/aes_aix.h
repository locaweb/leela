/*
aes_aix.h version 20050205
D. J. Bernstein
Public domain.
*/

#ifndef AES_AIX_H
#define AES_AIX_H

extern void aes_aix(unsigned char out[16],
  const unsigned char k[16],
  const unsigned char n[16]);

#ifndef aes_implementation
#define aes_implementation "aes_aix"
#define aes aes_aix
#endif

#endif
