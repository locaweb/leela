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

#ifndef leela_naming_h__
#define leela_naming_h__

#include <pthread.h>
#include "lql.h"
#include "base.h"
#include "endpoint.h"

LIBLEELA_HEAD

typedef struct leela_naming_t leela_naming_t;

/*! A linked-list of endpoints */
typedef struct leela_naming_cluster_t
{
  size_t             size;
  leela_endpoint_t **endpoint;
} leela_naming_cluster_t;

/*! Intializes the naming thread. You must provide at least one
 * warpdrive node that will be used to discover information about the
 * cluster.
 *
 * When this module connects to leela, it uses a default username &
 * secret. These are "nobody" and "".
 *
 * If you want to override this, define two environment variables:
 *
 *    + LEELA_NAMING_USER="username"
 *    + LEELA_NAMING_PASS="hex-encoded-secret"
 *
 *  \param warpdrive The endpoint to discover information about the
 *  cluster. This last element of this list must be NULL;
 *  
 *  \param maxdelay The maximum amount of time to wait between
 *  consecute calls;
 *
 *  \return NULL      : there was an error and the naming could not be created;
 *  \return :otherwise: success;
 */
LIBLEELA_API leela_naming_t *leela_naming_init (const leela_endpoint_t *const *warpdrive, int maxdelay);

/*! Starts the discover thread. This functions waits for the naming
 *  thread to query a warpdrive instance.
 *
 *  \return true : succesfully connected to the warpdrive cluster;
 *  \return false: could not connect to any machine;
 */
LIBLEELA_API bool leela_naming_start (leela_naming_t *naming, lql_context_t *ctx);

/*! Returns the endpoints found under this resource.
 *
 *  \return NULL     : zero endpoints found;
 *  \return otherwise: A valid endpoint;
 */
LIBLEELA_API leela_naming_cluster_t *leela_naming_discover (leela_naming_t *);

/*! Returns a endpoint to use, so that the load is distruted uniformly
 *  amongst the cluster members.
 *
 *  \return NULL      : failure;
 *  \return otherwise : A valid endpoint to use;
 */
LIBLEELA_API leela_endpoint_t *leela_naming_select (leela_naming_t *);

/*! Frees memory
 */
LIBLEELA_API void leela_naming_cluster_free (leela_naming_cluster_t *);

/*! Terminates the naming thread. This functions waits for the naming
 *  thread to finish, so it may take up to a second to return.
 */
LIBLEELA_API void leela_naming_destroy (leela_naming_t *);

LIBLEELA_TAIL

#endif
