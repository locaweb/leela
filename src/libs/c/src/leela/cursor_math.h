/* Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
 *                    Andre Ferraz <deferraz@terra.com.br>
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

#ifndef __cursor_math_h__
#define __cursor_math_h__

typedef struct { 
  lql_cursor_t *cursor1;
  lql_cursor_t *cursor2;
} lql_meta_cursor_t;

lql_meta_cursor_t *leela_new_meta_cursor(lql_cursor_t *cursor1, lql_cursor_t *cursor2);

leela_status leela_lql_meta_cursor_next (lql_meta_cursor_t *meta);

lql_tattr_t *leela_fetch_meta_cursor(lql_meta_cursor_t *meta, aggr_tattr_f);

typedef lql_value_t* (*aggr_tattr_f)(lql_value_t *, lql_value_t *);

#endif
