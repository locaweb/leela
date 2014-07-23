/*
cpucycles_athlon.h version 20050218
D. J. Bernstein
Public domain.
*/

#ifndef CPUCYCLES_ATHLON_H
#define CPUCYCLES_ATHLON_H

extern long long cpucycles_athlon(void);

#ifndef cpucycles_implementation
#define cpucycles_implementation "cpucycles_athlon"
#define cpucycles cpucycles_athlon
#endif

#endif
