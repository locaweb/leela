/*
poly1305aes-speed.c version 20050218
D. J. Bernstein
Public domain.
*/

#include <time.h>
#include <sys/time.h>
#include <stdio.h>
#include "poly1305aes.h"
#include "cpucycles.h"

char flushbuf[8388608];

#define FUN(x) int x(int i) { int j = i + flushbuf[0]; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN2(x) \
  FUN(x ## 0) \
  FUN(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN4(x) \
  FUN2(x ## 0) \
  FUN2(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN8(x) \
  FUN4(x ## 0) \
  FUN4(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN16(x) \
  FUN8(x ## 0) \
  FUN8(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN32(x) \
  FUN16(x ## 0) \
  FUN16(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN64(x) \
  FUN32(x ## 0) \
  FUN32(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN128(x) \
  FUN64(x ## 0) \
  FUN64(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN256(x) \
  FUN128(x ## 0) \
  FUN128(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN512(x) \
  FUN256(x ## 0) \
  FUN256(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

#define FUN1024(x) \
  FUN512(x ## 0) \
  FUN512(x ## 1) \
  int x(int i) { int j = i + x ## 0(i) + x ## 1(i); \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; i += j; j ^= i; \
  return j; }

FUN1024(icacheflush)

void cacheflush(void)
{
  int i = 0;
  int c0;
  int c1;
  int c2;
  int c3;
  int loop;
  flushbuf[0] = icacheflush(0);
  for (loop = 0;loop < 3;++loop)
    while (i < 8388608) {
      c0 = flushbuf[i];
      c1 = flushbuf[i + 64];
      c2 = flushbuf[i + 128];
      c3 = flushbuf[i + 192];
      flushbuf[i + 1] = c3;
      flushbuf[i + 65] = c2;
      flushbuf[i + 129] = c1;
      flushbuf[i + 193] = c0;
      i += 256;
    }
  flushbuf[0] = icacheflush(0);
}

long long tstart;  struct timeval tvstart;
long long tfinish; struct timeval tvfinish;

long long t[21];
long long buf[1048576];

main()
{
  int len;
  unsigned char *kr;
  unsigned char *n;
  unsigned char *m;
  unsigned char *a;
  int keygap;
  int datagap;
  int i;
  int j;

  printf("%s %s\n",poly1305aes_implementation,cpucycles_implementation);
  tstart = cpucycles(); gettimeofday(&tvstart,0);

  for (i = 0;i < sizeof buf;++i) i[(char *) buf] = random();

#define DOIT(cachedkeys,cacheddata,aligned,fun,funsymbol) \
  kr = (unsigned char *) buf; \
  if (!aligned) ++kr; \
  n = kr + 1024; \
  a = kr + 2048; \
  m = kr + 3072; \
  if (!cachedkeys) \
    keygap = 65536; \
  else \
    keygap = 32; \
  if (!cacheddata) \
    datagap = 262144; \
  else \
    datagap = 0; \
  for (i = 0;i <= 20;++i) { \
    for (j = 0;j < 32;++j) kr[j] = random(); \
    for (j = 0;j < 16;++j) n[j] = random(); \
    for (j = 0;j < 16;++j) a[j] = random(); \
    for (j = 0;j < len;++j) m[j] = random(); \
    poly1305aes_clamp(kr); \
    kr += keygap; \
    n += datagap; \
    m += datagap; \
    a += datagap; \
  } \
  cacheflush(); \
  kr = (unsigned char *) buf; \
  if (!aligned) ++kr; \
  n = kr + 1024; \
  a = kr + 2048; \
  m = kr + 3072; \
  keygap = cachedkeys ? 32 : 262144; \
  datagap = cacheddata ? 0 : 262144; \
  if (cachedkeys) { \
    for (i = 0;i <= 20;++i) { \
      for (j = 0;j < 32;++j) flushbuf[j] ^= kr[j]; \
      kr += keygap; \
    } \
    for (i = 0;i <= 20;++i) kr -= keygap; \
  } \
  if (cacheddata) { \
    for (j = 0;j < 16;++j) flushbuf[j] ^= n[j]; \
    for (j = 0;j < 16;++j) flushbuf[j] ^= a[j]; \
    for (j = 0;j < len;++j) flushbuf[j] ^= m[j]; \
  } \
  for (i = 0;i <= 20;++i) t[i] = cpucycles(); \
  for (i = 0;i <= 20;++i) { \
    t[i] = cpucycles(); \
    fun(a,kr,n,m,len); \
    kr += keygap; \
    n += datagap; \
    m += datagap; \
    a += datagap; \
  } \
  printf("%4d ",len); \
  printf(funsymbol); \
  printf(aligned ? "4444" : "0000"); \
  printf(cachedkeys ? "K" : "-"); \
  printf(cacheddata ? "D" : "-"); \
  for (i = 0;i < 20;++i) printf(" %5lld",t[i + 1] - t[i]); \
  printf("\n"); \
  fflush(stdout);

#define DOIT3(cachedkeys,cacheddata,aligned,fun,funsymbol) \
  DOIT(cachedkeys,cacheddata,aligned,fun,funsymbol) \
  DOIT(cachedkeys,cacheddata,aligned,fun,funsymbol) \
  DOIT(cachedkeys,cacheddata,aligned,fun,funsymbol)

#define DOIT6(cachedkeys,cacheddata,aligned) \
  DOIT3(cachedkeys,cacheddata,aligned,poly1305aes_authenticate,"A") \
  DOIT3(cachedkeys,cacheddata,aligned,poly1305aes_verify,"V")

#define DOIT12(cachedkeys,cacheddata) \
  DOIT6(cachedkeys,cacheddata,1) \
  DOIT6(cachedkeys,cacheddata,0)

  for (len = 0;len <= 8192;++len) {
    DOIT12(1,1)
    DOIT12(0,1)
    DOIT12(1,0)
    DOIT12(0,0)
  }

  tfinish = cpucycles(); gettimeofday(&tvfinish,0);
  printf("%lld cycles\n"
    ,tfinish - tstart);
  printf("%lld usecs\n"
    ,(tvfinish.tv_sec - tvstart.tv_sec) * 1000000LL + (tvfinish.tv_usec - tvstart.tv_usec));
  printf("%f MHz\n"
    ,(tfinish - tstart) * 1.0 / ((tvfinish.tv_sec - tvstart.tv_sec) * 1000000LL + (tvfinish.tv_usec - tvstart.tv_usec)));

  return 0;
}
