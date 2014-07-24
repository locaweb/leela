/*
cpucycles_zero.h version 20050218
D. J. Bernstein
Public domain.
*/

#ifndef CPUCYCLES_ZERO_H
#define CPUCYCLES_ZERO_H

extern long long cpucycles_zero(void);

#ifndef cpucycles_implementation
#define cpucycles_implementation "cpucycles_zero"
#define cpucycles cpucycles_zero
#endif

#endif
