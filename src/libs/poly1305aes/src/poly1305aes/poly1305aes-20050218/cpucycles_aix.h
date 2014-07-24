/*
cpucycles_aix.h version 20050205
D. J. Bernstein
Public domain.
*/

#ifndef CPUCYCLES_AIX_H
#define CPUCYCLES_AIX_H

extern long long cpucycles_aix(void);

#ifndef cpucycles_implementation
#define cpucycles_implementation "cpucycles_aix"
#define cpucycles cpucycles_aix
#endif

#endif
