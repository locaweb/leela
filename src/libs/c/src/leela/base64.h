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

#ifndef leela_base64_h__
#define leela_base64_h__

#include <stdlib.h>
#include "base.h"

LIBLEELA_HEAD

/*! The first 64 characters are the alphabet. The last one is the
 *  padding.
 */
extern const char default_alphabet[65];

/*! performs the base64 encode.
 *
 * \param dst The destination variable. This variable must be at least
 *            ceil(n/3.0) * 4 + 1 times bigger than the src variable. This
 *            variable may be NULL if dstlen is 0;
 *
 * \param dstlen The size of dst variable;
 *
 * \param src The [maybe binary] string to encode;
 *
 * \param srclen The size of the src variable;
 * 
 * \return how many bytes were written to dst variable (this may be
 *         bigger than dstlen);
 */
LIBLEELA_API size_t leela_b64encode (const char alphabet[65], char *dst, size_t dstlen, const char *src, size_t srclen);

/*! Decodes a base64 string.
 *
 * \param dst The destination variable. This variable must be at least
 *            ceil(n/4.0) * 3 + 1. This variable may be NULL if dstlen
 *            is 0;
 *
 * \param dstlen The size of dst variable;
 *
 * \param src The base64 encoded string;
 *
 * \param srclen The size of the src variable;
 *
 * \return how many bytes were written to dst variable (this may be
 *         bigger than dstlen);
 */
LIBLEELA_API size_t leela_b64decode (char *dst, size_t dstlen, const char *src, size_t srclen);

LIBLEELA_TAIL

#endif
