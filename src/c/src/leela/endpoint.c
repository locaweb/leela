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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "leela/string.h"
#include "leela/endpoint.h"

static
void __free_addr(leela_addr_t *addr)
{
  if (addr != NULL)
  { free(addr->host); }
}

static
size_t __count_addrs(const char *endpoint)
{
  size_t found = 1;
  for (size_t k = 0; endpoint[k] != '\0'; k += 1)
  {
    if (endpoint[k] == ',')
    { found += 1; }
    if (endpoint[k] == '/' || endpoint[k] == ';')
    { break; }
  }
  return(found);
}

leela_endpoint_t *leela_endpoint_load(const char *endpoint)
{
  leela_endpoint_t *result = (leela_endpoint_t *) malloc(sizeof(leela_endpoint_t));
  if (result == NULL)
  { return(NULL); }

  result->addrlen = 0;
  result->addrs   = NULL;
  result->path    = NULL;
  if (strncmp("tcp://", endpoint, 6) == 0)
  { result->protocol = PROTO_TCP; }
  else if (strncmp("udp://", endpoint, 6) == 0)
  { result->protocol = PROTO_UDP; }
  else
  { goto handle_error; }

  size_t k;
  size_t at       = 0;
  size_t start    = 6;
  result->addrlen = __count_addrs(endpoint + start);
  result->addrs   = (leela_addr_t *) malloc(sizeof(leela_addr_t) * result->addrlen);
  if (result->addrs == NULL)
  { goto handle_error; }
  for (k = 0; k < result->addrlen; k += 1)
  { result->addrs[k].host = NULL; }
  for (k = start; endpoint[k] != '\0'; k += 1)
  {
    if (endpoint[k] == ':')
    {
      result->addrs[at].host = leela_strndup(endpoint + start, k - start);
      if (result->addrs[at].host == NULL)
      { goto handle_error; }
    }
    else if (endpoint[k] == ',' || endpoint[k] == ';' || endpoint[k] == '/')
    {
      at        += 1;
      char *port = leela_strndup(endpoint + start, k - start);
      if (port == NULL)
      { goto handle_error; }
      result->addrs[at-1].port = atoi(port);
      free(port);
    }

    if (endpoint[k] == ';' || endpoint[k] == '/')
    { break; }
    else if (endpoint[k] == ',' || endpoint[k] == ':')
    { start = k + 1; }
  }

  if (endpoint[k] == '/')
  {
    size_t slen  = strlen(endpoint + k);
    result->path = leela_strndup(endpoint + k, slen - 1);
  }
  else if (endpoint[k] == ';')
  { result->path = leela_strdup(""); }
  if (result->path == NULL)
  { goto handle_error; }

  return(result);

handle_error:
  leela_endpoint_free(result);
  return(NULL);
}

leela_endpoint_t *leela_endpoint_dup(const leela_endpoint_t *endpoint)
{
  leela_endpoint_t *dup = (leela_endpoint_t *) malloc (sizeof(leela_endpoint_t));
  if (dup == NULL)
  { return(NULL); }
  dup->protocol = endpoint->protocol;
  dup->addrlen  = endpoint->addrlen;
  dup->path     = leela_strdup(endpoint->path);
  dup->addrs    = (leela_addr_t *) malloc(sizeof(leela_addr_t) * endpoint->addrlen);
  if (dup->addrs != NULL)
  {
    for (size_t k = 0; k < endpoint->addrlen; k += 1)
    { dup->addrs[k].host = NULL; }

    for (size_t k = 0; k < endpoint->addrlen; k += 1)
    {
      dup->addrs[k].port = endpoint->addrs[k].port;
      dup->addrs[k].host = leela_strdup(endpoint->addrs[k].host);
      if (dup->addrs[k].host == NULL)
      {
        leela_endpoint_free(dup);
        return(NULL);
      }
    }
  }

  if (dup->addrs == NULL || dup->path == NULL)
  {
    leela_endpoint_free(dup);
    return(NULL);
  }

  return(dup);
}

char *leela_endpoint_dump(const leela_endpoint_t *endpoint)
{
  size_t at = 0;
  size_t l  = 6 + 1;                                // protocol + semi-colon
  l       += strlen(endpoint->path);                // path
  for (size_t k = 0; k < endpoint->addrlen; k += 1)
  { l += strlen(endpoint->addrs[k].host) + 1 + 5; } // host + colon + port

  char *s = (char *) malloc(l);
  if (s == NULL)
  { return(s); }

  at += snprintf(s, l, "tcp://");
  for (size_t k = 0; k < endpoint->addrlen; k += 1)
  { at += snprintf(s + at, l - at, "%s:%d,", endpoint->addrs[k].host, endpoint->addrs[k].port); }
  at -= 1; // removes the trailing comma
  snprintf(s + at, l - at, "%s;", endpoint->path);

  return(s);
}

void leela_endpoint_free(leela_endpoint_t *endpoint)
{
  if (endpoint == NULL)
  { return; }
  while (endpoint->addrlen-- > 0)
  { __free_addr(endpoint->addrs + endpoint->addrlen); }
  free(endpoint->path);
  free(endpoint->addrs);
  free(endpoint);
}
