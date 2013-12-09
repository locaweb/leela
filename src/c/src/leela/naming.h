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

#ifndef __leela_naming_h__
#define __leela_naming_h__

#include "leela/endpoint.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct leela_resolver_t leela_resolver_t;

//! A linked-list of endpoints
typedef struct leela_resolver_value_t
{
  leela_endpoint_t              *endpoint;
  struct leela_resolver_value_t *next;
} leela_resolver_value_t;

/*! Intializes the resolver thread. This constantly queries zookeeper
 *  for updates about this particular resource.
 *
 *  \param zookeeper The endpoint for the zookeeper cluster to use;
 *  
 *  \param resource The resource to monitor;
 *
 *  \return NULL      : there was an error and the resolver could not be created;
 *  \return :otherwise: success;
 */
leela_resolver_t *leela_resolver_init(const leela_endpoint_t *zookeeper, const char *resource);

/*! Returns the endpoints found under this resource;
 *
 *  \return NULL     : zero endpoints found;
 *  \return otherwise: A valid endpoint;
 */
leela_resolver_value_t *leela_resolver_query(leela_resolver_t *);

/*! Frees memory;
 */
void leela_resolver_value_free(leela_resolver_value_t *);

/*! Terminates the resolver thread.
 *
 * \param The last value found on zookeeper. This may be NULL.
 */
void leela_resolver_shutdown(leela_resolver_t *, leela_resolver_value_t **result);

#ifdef __cplusplus
}
#endif

#endif
