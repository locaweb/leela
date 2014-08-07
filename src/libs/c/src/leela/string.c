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

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include "string.h"

struct leela_strbuilder_t
{
  char *buffer;
  size_t len;
  size_t off;
};

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

void leela_strbuilder_free (leela_strbuilder_t *builder)
{
  if (builder != NULL)
  {
    free(builder->buffer);
    free(builder);
  }
}

leela_strbuilder_t *leela_strbuilder_realloc (leela_strbuilder_t *builder, size_t len)
{
  builder->off = 0;

  if (len == 0)
  {
    free(builder->buffer);
    builder->len    = 0;
    builder->buffer = NULL;
  }
  else
  {
    char *newbuffer = (char *) realloc(builder->buffer, len + 2);
    if (newbuffer == NULL)
    {
      leela_strbuilder_free(builder);
      return(NULL);
    }
    builder->len    = len + 1;
    builder->buffer = newbuffer;
    memset(builder->buffer, '\0', len + 2);
  }
  return(builder);
}

leela_strbuilder_t *leela_strbuilder_new (size_t len)
{
  leela_strbuilder_t *builder = (leela_strbuilder_t *) malloc(sizeof(leela_strbuilder_t));
  if (builder != NULL)
  {
    builder->len    = len;
    builder->off    = 0;
    builder->buffer = NULL;
    builder         = leela_strbuilder_realloc(builder, len);
  }
  return(builder);
}

size_t leela_strbuilder_strlen (leela_strbuilder_t *builder)
{ return(builder->off); }

const char *leela_strbuilder_string (leela_strbuilder_t *builder)
{ return(builder->buffer); }

int leela_strbuilder_add_str (leela_strbuilder_t *builder, const char *str)
{ return(leela_strbuilder_add_nstr(builder, str, strlen(str))); }

int leela_strbuilder_add_nstr (leela_strbuilder_t *builder, const char *str, size_t len)
{
  if (builder->buffer != NULL)
  {
    if (len + builder->off >= builder->len)
    { return(-1); }
    strncpy(builder->buffer + builder->off, str, len);
  }
  builder->off += len;
  return(0);
}

int leela_strbuilder_add_fmt (leela_strbuilder_t *builder, const char *fmt, ...)
{
  int off;
  size_t len = builder->len > builder->off ? builder->len - builder->off : 0;
  va_list args;
  va_start(args, fmt);
  off = vsnprintf(builder->buffer == NULL ? NULL : builder->buffer + builder->off, len, fmt, args);
  va_end(args);
  if (off < 0 || (size_t) off > len)
  { return(-1); }
  builder->off += off;
  return(0);
}
