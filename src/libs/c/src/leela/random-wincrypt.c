/* Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifdef HAS_SYM_WINCRYPT

#pragma comment(lib, "crypt32.lib")

#include <windows.h>
#include <wincrypt.h>
#include "random.h"

struct leela_random_t
{
  HCRYPTPROV provider;
};

int leela_random_read (leela_random_t *state, void *buffer, size_t bytes)
{
  if (CryptGenRandom(state->provider, bytes, (BYTE *) buffer))
  { return(0); }
  return(-1);
}

leela_random_t *leela_random_init ()
{
  leela_random_t *provider = (leela_random_t *) malloc(sizeof(leela_random_t));
  if (CryptAcquireContext(&provider->provider, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT))
  {
    unsigned int seed;
    if (leela_random_read(provider, &seed, sizeof(seed)) == 0)
    {
      srand(seed);
      return(provider);
    }
  }
  return(NULL);
}

int leela_random_next (leela_random_t *state, long int *random)
{ return(leela_random_read(state, &random, sizeof(random))); }

#else

/* XXX: ISO C forbids an empty translation unit */
static
void leela_random_dummy__()
{}

#endif
