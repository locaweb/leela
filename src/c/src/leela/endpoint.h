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

#ifndef __leela_endpoint_h__
#define __leela_endpoint_h__

#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
  char     *host;
  uint16_t  port;
} leela_addr_t;

typedef enum leela_protocol
{
  PROTO_TCP,
  PROTO_UDP
} leela_protocol;

typedef struct
{
  leela_protocol    protocol;
  size_t            addrlen;  //!^ The number of `addr' items;
  leela_addr_t     *addrs;
  char             *path;
} leela_endpoint_t;

/*! Parses an endpoint. The syntax this parses recognizes is the
 *  following:
 *      tcp|udp://host_0:port_0[,...,host_n:port_n][/path]
 *
 *  Example:
 *      tcp://foo:80,foo:81,foo:82/root
 *
 *  N.B.:
 *      The maximum number of components is 512;
 *
 *  \param endpoint The string to parse;
 *  
 *  \return NULL      : the endpoint could not be parsed;
 *  \return :otherwise: the parsed endpoint;
 */
leela_endpoint_t *leela_endpoint_load(const char *endpoint);

/*! Transforms an endpoint into a string. This forms an identity with
 *  `leela_endpoint_load' function: `load(dump(t)) == t';
 *
 *  N.B.: remember to free the returned memory (which might be NULL);
 */
char *leela_endpoint_dump(const leela_endpoint_t *);

/*! Duplicates an endpoint;
 */
leela_endpoint_t *leela_endpoint_dup(const leela_endpoint_t *);

/*! Frees memory.
 */
void leela_endpoint_free(leela_endpoint_t *);

#ifdef __cplusplus
}
#endif

#endif
