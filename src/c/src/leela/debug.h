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

#ifndef __leela_debug_h__
#define __leela_debug_h__
#include "lql.h"

#define LEELA_DEBUG0(ctx, fmt) lql_log(ctx, "%s:%d: " #fmt , __FILE__, __LINE__)

#define LEELA_DEBUG(ctx, fmt, ...) lql_log(ctx, "%s:%d: " #fmt , __FILE__, __LINE__, __VA_ARGS__)

#ifdef LEELA_TRACING
# define LEELA_TRACE0(ctx, fmt) lql_log(ctx, "[TRACE] %s:%d: " #fmt, __FILE__, __LINE__)
# define LEELA_TRACE(ctx, fmt, ...) lql_log(ctx, "[TRACE] %s:%d: " #fmt, __FILE__, __LINE__, __VA_ARGS__)
#else
# define LEELA_TRACE0(ctx, fmt)
# define LEELA_TRACE(ctx, fmt, ...)
#endif

#endif
