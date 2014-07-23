/*
cpucycles_ppro.h version 20050213
D. J. Bernstein
Public domain.
*/

#ifndef CPUCYCLES_PPRO_H
#define CPUCYCLES_PPRO_H

extern long long cpucycles_ppro(void);

#ifndef cpucycles_implementation
#define cpucycles_implementation "cpucycles_ppro"
#define cpucycles cpucycles_ppro
#endif

#endif
