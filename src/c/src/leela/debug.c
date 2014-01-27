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
#include <stdarg.h>
#include <pthread.h>
#include "leela/debug.h"

static pthread_mutex_t debug_mutex = PTHREAD_MUTEX_INITIALIZER;

void leela_debug(const char *fmt, ...)
{
  if (pthread_mutex_lock(&debug_mutex) == 0)
  {
    va_list argp;
    va_start(argp, fmt);
    fprintf(stderr, "[DEBUG] ");
    vfprintf(stderr, fmt, argp);
    fprintf(stderr, "\n");
    va_end(argp);
    pthread_mutex_unlock(&debug_mutex);
  }
}
