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

#ifndef HAS_SYM_WINCRYPT

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "random.h"

struct leela_random_t
{
  FILE *fh;
};

int leela_random_read (leela_random_t *state, void *buffer, size_t bytes)
{
  size_t sz = sizeof(char);
  size_t nb = bytes;
  if (state->fh != NULL)
  {
    size_t rc = fread(buffer, sz, nb, state->fh);
    if (rc == nb)
    { return(0); }
  }
  return(-1);
}

leela_random_t *leela_random_init ()
{
  unsigned int seed     = 0;
  FILE *fh              = fopen("/dev/urandom", "r");
  leela_random_t *state = (leela_random_t *) malloc(sizeof(leela_random_t));
  if (state != NULL)
  {
    state->fh = fh;
    if (leela_random_read(state, &seed, sizeof(seed)) == 0)
    {
      srand(seed);
      srand48(seed);
      return(state);
    }
  }
  leela_random_close(state);
  return(NULL);
}

int leela_random_next (leela_random_t *unused, long int *random)
{
  if (unused == NULL)
  { return(-1); }

  *random = lrand48();
  return(0);
}

void leela_random_close (leela_random_t *data)
{
  if (data != NULL)
  {
    if (data->fh != NULL)
    { fclose(data->fh); }
    free(data);
  }
}

#else

/* XXX: ISO C forbids an empty translation unit */
static
void leela_random_dummy__()
{}

#endif
