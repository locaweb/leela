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

#ifndef __leela_status_h__
#define __leela_status_h__

typedef enum leela_status
{
  LEELA_OK       = 0,
  LEELA_EOF      = 1,
  LEELA_BADARGS  = 2,
  LEELA_TIMEOUT  = 3,
  LEELA_ERROR    = -1
} leela_status;

#endif
