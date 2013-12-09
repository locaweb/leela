/* This file is part of Leela.
 *
 * Leela is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Leela is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Leela.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include "leela/string.h"

char *leela_strdup(const char *s)
{ return(leela_strndup(s, strlen(s))); }

char *leela_strndup(const char *s, size_t l)
{
  char *d = (char *) malloc(l + 1);
  if (d != NULL)
  { strncpy(d, s, l); }
  d[l] = '\0';
  return(d);
}

char *leela_join(const char *base, ...)
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
