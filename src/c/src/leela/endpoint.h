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

#ifndef __leela_endpoint_h__
#define __leela_endpoint_h__

#include <stdlib.h>
#include <inttypes.h>
#include "leela/base.h"

LEELA_CPLUSPLUS_OPEN

typedef enum leela_protocol
{
  PROTO_TCP,
  PROTO_UDP
} leela_protocol;

typedef struct
{
  leela_protocol    protocol;
  char             *host;
  uint16_t          port;
  char             *path;
} leela_endpoint_t;

/*! Parses an endpoint. The syntax this parses recognizes is the
 *  following:
 *      tcp|udp://host:port[/path]
 *
 *  Example:
 *      tcp://localhost:4080/root
 *
 *  \param endpoint The string to parse;
 *  
 *  \return NULL      : the endpoint could not be parsed;
 *  \return :otherwise: the parsed endpoint;
 */
leela_endpoint_t *leela_endpoint_load (const char *endpoint);

/*! Transforms an endpoint into a string. This forms an identity with
 *  `leela_endpoint_load' function: `load(dump(t)) == t';
 *
 *  N.B.: remember to free the returned memory (which might be NULL);
 */
char *leela_endpoint_dump (const leela_endpoint_t *);

/*! Duplicates an endpoint;
 */
leela_endpoint_t *leela_endpoint_dup (const leela_endpoint_t *);

/*! Duplicates an endpoint;
 */
leela_endpoint_t *leela_endpoint_dup2 (leela_endpoint_t *dst, const leela_endpoint_t *src);

/*! Frees memory.
 */
void leela_endpoint_free (leela_endpoint_t *);

LEELA_CPLUSPLUS_CLOSE

#endif
