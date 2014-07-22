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

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include "leela/string.h"

char *leela_strdup (const char *s)
{ return(leela_strndup(s, strlen(s))); }

char *leela_strndup (const char *s, size_t l)
{
  char *d = (char *) malloc(l + 1);
  if (d != NULL)
  { strncpy(d, s, l); }
  d[l] = '\0';
  return(d);
}

char *leela_join (const char *base, ...)
{
  size_t baselen   = strlen(base);
  size_t offset    = baselen;
  char *str        = NULL;
  const char *item = NULL;

  va_list args;
  va_start(args, base);
  while ((item = va_arg(args, const char *)) != NULL)
  { baselen += strlen(item); }
  va_end(args);

  str = (char *) malloc(baselen + 1); 
  if (str == NULL)
  { return(NULL); }

  va_start(args, base);
  strcpy(str, base);
  while ((item = va_arg(args, char*)) != NULL)
  {
    if (strcmp(item, "") == 0)
    { continue; }
    strcpy(str+offset, item);
    offset += strlen(item);
  }
  str[offset] = '\0';
  va_end(args);

  return(str);
}

int leela_check_guid(const char *s)
{
  int k;
  if (strlen(s) != 36)
  { return(-1); }

  if (s[8] != '-' || s[13] != '-' || s[18] != '-' || s[23] != '-')
  { return(-1); }

  for (k=0; k<36; k+=1)
  {
    if ((s[k] < 0x30 || s[k] > 0x39)
        && (s[k] < 0x61 || s[k] > 0x66)
        && s[k] != '-')
    { return(-1); }
  }
  return(0);
}
