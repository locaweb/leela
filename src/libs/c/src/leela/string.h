/* Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http: *www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef leela_string_h__
#define leela_string_h__

#include <stdlib.h>
#include "base.h"

LIBLEELA_HEAD

typedef struct leela_strbuilder_t leela_strbuilder_t;

/*! Rezises the internal buffer of this builder.
 *
 * Notice this will **erase** all previous data in this builder, even
 * if the new size is greater than the previous size.
 *
 * \param builder A valid pointer to a builder type
 *
 * \param len The new size
 *
 * \return NULL: error
 *         xxxx: success
 */
LIBLEELA_API leela_strbuilder_t *leela_strbuilder_realloc (leela_strbuilder_t *builder, size_t len);

/*! Creates a new builder type.
 *
 * \param len The size of the internal buffer. This may be 0, in which
 * case no memory gets allocated.
 */
LIBLEELA_API leela_strbuilder_t *leela_strbuilder_new (size_t len);

LIBLEELA_API void leela_strbuilder_free (leela_strbuilder_t *builder);

/*! Append a NULL-terminated string to the internal buffer.
 *
 * This is equivalent to:
 *
 *     leela_strbuilder_add_nstr(builder, str, strlen(str));
 *
 * \param str The string to append;
 *
 * \return 0: success;
 *         x: failure (e.g.: not enough space);
 */
LIBLEELA_API int leela_strbuilder_add_str (leela_strbuilder_t *builder, const char *str);

/*! Returns the built string
 */
LIBLEELA_API const char *leela_strbuilder_string (leela_strbuilder_t *builder);

/*! Returns the length of the string
 */
LIBLEELA_API size_t leela_strbuilder_stringlen (leela_strbuilder_t *builder);

/*! Append a string to the internal buffer.
 *
 * \param str The string to append;
 *
 * \param len the size of the str argument;
 *
 * \return 0: success;
 *         x: failure (e.g.: not enough space);
 */
LIBLEELA_API int leela_strbuilder_add_nstr (leela_strbuilder_t *builder, const char *str, size_t len);

/*! Append a string to the internal buffer using printf-like syntax.
 *
 * \return 0: success;
 *         x: failure (e.g.: not enough space);
 */
LIBLEELA_API int leela_strbuilder_add_fmt (leela_strbuilder_t *builder, const char *fmt, ...);

LIBLEELA_API char *leela_strdup (const char *);

LIBLEELA_API char *leela_strndup (const char *, size_t);

LIBLEELA_API char *leela_join (const char *, ...);

/*! Check if the string is a valid guid.
 *
 * \return 0 Ok;
 * \return x String is not a guid;
 */
LIBLEELA_API int leela_check_guid(const char *);

LIBLEELA_TAIL

#endif
