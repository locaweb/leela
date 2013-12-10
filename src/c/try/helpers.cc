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

#include <cstdlib>
#include <string.h>
#include <zookeeper/zookeeper.h>
#include "helpers.hh"
#include "leela/string.h"

void test_leela_zk_rmrf(zhandle_t *zh, const char *path)
{
  struct String_vector children;
  if (zoo_get_children(zh, path, 0, &children) == ZOK)
  {
    for (int k=0; k<children.count; k+=1)
    {
      char *child = leela_join(path, "/", children.data[k], NULL);
      test_leela_zk_rmrf(zh, child);
      free(child);
    }
  }
  zoo_delete(zh, path, -1);
}

void test_leela_zk_write(zhandle_t *zh, const char *path, const char *data)
{ zoo_create(zh, path, data, (data == NULL ? 0 : strlen(data) + 1), &ZOO_OPEN_ACL_UNSAFE, 0, NULL, 0); }
