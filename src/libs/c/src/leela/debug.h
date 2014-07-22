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

#ifndef __leela_debug_h__
#define __leela_debug_h__
#include "lql.h"

#define LEELA_DEBUG0(ctx, fmt) lql_debug(ctx, "[debug] %s:%d: " #fmt , __FILE__, __LINE__)
#define LEELA_DEBUG1(ctx, fmt, arg0) lql_debug(ctx, "[debug] %s:%d: " #fmt , __FILE__, __LINE__, arg0)
#define LEELA_DEBUG2(ctx, fmt, arg0, arg1) lql_debug(ctx, "[debug] %s:%d: " #fmt , __FILE__, __LINE__, arg0, arg1)
#define LEELA_DEBUG3(ctx, fmt, arg0, arg1, arg2) lql_debug(ctx, "[debug] %s:%d: " #fmt , __FILE__, __LINE__, arg0, arg1, arg2)

#define LEELA_TRACE0(ctx, fmt) lql_trace(ctx, "[trace] %s:%d: " #fmt, __FILE__, __LINE__)
#define LEELA_TRACE1(ctx, fmt, arg0) lql_trace(ctx, "[trace] %s:%d: " #fmt, __FILE__, __LINE__, arg0)
#define LEELA_TRACE2(ctx, fmt, arg0, arg1) lql_trace(ctx, "[trace] %s:%d: " #fmt, __FILE__, __LINE__, arg0, arg1)
#define LEELA_TRACE3(ctx, fmt, arg0, arg1, arg2) lql_trace(ctx, "[trace] %s:%d: " #fmt, __FILE__, __LINE__, arg0, arg1, arg2)

#endif
