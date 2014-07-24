/*
cpucycles_macos.h version 20050207
D. J. Bernstein
Public domain.
*/

#ifndef CPUCYCLES_MACOS_H
#define CPUCYCLES_MACOS_H

extern long long cpucycles_macos(void);

#ifndef cpucycles_implementation
#define cpucycles_implementation "cpucycles_macos"
#define cpucycles cpucycles_macos
#endif

#endif
