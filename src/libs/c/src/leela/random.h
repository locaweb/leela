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

#ifndef leela_random_h__
#define leela_random_h__

#include "base.h"

LIBLEELA_HEAD

typedef struct leela_random_t leela_random_t;

/*! Initializes the random with a suitable seed so that
 *  leela_random_next can be used.
 *
 *  \return NULL: an error has occurred;
 *          x   : ok;
 */
LIBLEELA_API leela_random_t *leela_random_init ();

/*! Read a given amount of random bytes from a suitable source.
 *
 * \return 0: ok;
 *         x: an error has ocurred;
 */
LIBLEELA_API int leela_random_read (leela_random_t *, void *buffer, size_t bytes);

/*! Returns the next random number.
 *
 * \param random The variable that receives the next random number
 * 
 * \return 0: ok;
 *         x: an error has ocurred;
 */
LIBLEELA_API int leela_random_next (leela_random_t *, long int *random);

LIBLEELA_API void leela_random_close (leela_random_t *);

LIBLEELA_TAIL

#endif
