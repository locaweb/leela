// Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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
#include <stdbool.h>
#include "leela/string.h"
#include "leela/endpoint.h"

leela_endpoint_t *leela_endpoint_load(const char *endpoint)
{
  leela_endpoint_t *result = (leela_endpoint_t *) malloc(sizeof(leela_endpoint_t));
  if (result == NULL)
  { return(NULL); }

  result->host = NULL;
  result->path = NULL;
  if (strncmp("tcp://", endpoint, 6) == 0)
  { result->protocol = PROTO_TCP; }
  else if (strncmp("udp://", endpoint, 6) == 0)
  { result->protocol = PROTO_UDP; }
  else
  { goto handle_error; }

  size_t k     = 6;
  size_t start = k;
  while (true)
  {
    if (endpoint[k] == ':')
    {
      result->host = leela_strndup(endpoint + start, k - start);
      if (result->host == NULL)
      { goto handle_error; }
      start = k + 1;
    }
    else if (endpoint[k] == '\0' || endpoint[k] == '/')
    {
      char *port = leela_strndup(endpoint + start, k - start);
      if (port == NULL)
      { goto handle_error; }
      result->port = atoi(port);
      free(port);
      break;
    }

    k += 1;
  }
  result->path = leela_strdup(endpoint + k);
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
  if (leela_endpoint_dup2(dup, endpoint) == NULL)
  { free(dup); }
  return(dup);
}

leela_endpoint_t *leela_endpoint_dup2(leela_endpoint_t *dup, const leela_endpoint_t *endpoint)
{
  dup->protocol = endpoint->protocol;
  dup->host     = leela_strdup(endpoint->host);
  dup->port     = endpoint->port;
  dup->path     = leela_strdup(endpoint->path);

  if (dup->host == NULL || dup->path == NULL)
  {
    free(dup->host);
    free(dup->path);
    dup->host = NULL;
    dup->path = NULL;
    return(NULL);
  }

  return(dup);
}

char *leela_endpoint_dump(const leela_endpoint_t *endpoint)
{
  size_t l  = 6 + 5 + 1; // protocol + port + \0
  l        += strlen(endpoint->host);               
  l        += strlen(endpoint->path);
  char *s   = (char *) malloc(l);
  if (s == NULL)
  { return(s); }

  snprintf(s, l, "tcp://%s:%d%s", endpoint->host, endpoint->port, endpoint->path);
  return(s);
}

void leela_endpoint_free(leela_endpoint_t *endpoint)
{
  if (endpoint == NULL)
  { return; }
  free(endpoint->host);
  free(endpoint->path);
  free(endpoint);
}
