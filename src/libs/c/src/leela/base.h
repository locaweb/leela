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

#ifndef leela_base_h__
#define leela_base_h__

#ifdef __cplusplus
#  define LIBLEELA_HEAD extern "C" {
#  define LIBLEELA_TAIL }
#else
#  define LIBLEELA_HEAD
#  define LIBLEELA_TAIL
#endif

#ifdef _WIN32
#  if defined(LIBLEELA_STATIC)
#    define LIBLEELA_API
#  elif defined(LIBLEELA_SHARED)
#    define LIBLEELA_API __declspec(dllexport)
#  else
#    define LIBLEELA_API __declspec(dllimport)
#  endif
#else
#  define LIBLEELA_API
#endif

#define LEELA_MIN(a, b) (a < b ? a : b)
#define LEELA_MAX(a, b) (a < b ? b : a)

#endif
