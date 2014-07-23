/*
cpucycles_sparc.h version 20050201
D. J. Bernstein
Public domain.
*/

#ifndef CPUCYCLES_SPARC_H
#define CPUCYCLES_SPARC_H

extern long long cpucycles_sparc(void);

#ifndef cpucycles_implementation
#define cpucycles_implementation "cpucycles_sparc"
#define cpucycles cpucycles_sparc
#endif

#endif
