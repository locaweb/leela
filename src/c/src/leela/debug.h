// Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

#define LEELA_DEBUG0(fmt) leela_debug("%s:%d: " #fmt , __FILE__, __LINE__)

#define LEELA_DEBUG(fmt, ...) leela_debug("%s:%d: " #fmt , __FILE__, __LINE__, __VA_ARGS__)

#ifdef LEELA_TRACING
# define LEELA_TRACE0(fmt) leela_debug("[TRACE] %s:%d: " #fmt, __FILE__, __LINE__)
# define LEELA_TRACE(fmt, ...) leela_debug("[TRACE] %s:%d: " #fmt, __FILE__, __LINE__, __VA_ARGS__)
#else
# define LEELA_TRACE0(fmt)
# define LEELA_TRACE(fmt, ...)
#endif

void leela_debug(const char *fmt, ...);

#endif
