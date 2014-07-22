// Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "random.h"

int leela_random_urandom (void *buffer, size_t bytes)
{
  FILE *fh  = fopen("/dev/urandom", "r");
  size_t sz = sizeof(char);
  size_t nb = bytes;
  if (fh != NULL)
  {
    size_t rc = fread(buffer, sz, nb, fh);
    fclose(fh);
    if (rc == nb)
    { return(0); }
  }
  return(-1);
}
