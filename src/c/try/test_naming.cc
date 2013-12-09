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
#include <unistd.h>
#include <UnitTest++.h>
#include <zookeeper/zookeeper.h>
#include "leela/naming.h"
#include "leela/string.h"
#include "leela/endpoint.h"

static
void __zk_rmrf(zhandle_t *zh, const char *path)
{
  struct String_vector children;
  if (zoo_get_children(zh, path, 0, &children) == ZOK)
  {
    for (int k=0; k<children.count; k+=1)
    {
      char *child = leela_join(path, "/", children.data[k], NULL);
      __zk_rmrf(zh, child);
      free(child);
    }
  }
  zoo_delete(zh, path, -1);
}

static
void __zk_write(zhandle_t *zh, const char *path, const char *data)
{ zoo_create(zh, path, data, (data == NULL ? 0 : strlen(data) + 1), &ZOO_OPEN_ACL_UNSAFE, 0, NULL, 0); }

TEST(test_leela_naming__)
{
  zhandle_t *zh = zookeeper_init("localhost:2181", NULL, 60000, NULL, NULL, 0);
  __zk_rmrf(zh, "/leela-dev");
  __zk_write(zh, "/leela-dev", NULL);
  __zk_write(zh, "/leela-dev/child-0", "tcp://localhost:8080;");
  __zk_write(zh, "/leela-dev/child-1", "tcp://localhost:8081;");
  __zk_write(zh, "/leela-dev/child-2", "tcp://localhost:8082;");
  zookeeper_close(zh);

  leela_endpoint_t *endpoint     = leela_endpoint_load("tcp://localhost:2181;");
  leela_naming_t *naming         = leela_naming_init(endpoint, "/leela-dev");
  leela_naming_value_t *snapshot = NULL;
  leela_naming_shutdown(naming, &snapshot);
  leela_naming_value_t *item = snapshot;
  for (int k=0; k<3; k+=1)
  {
    CHECK(item != NULL);
    CHECK(item->endpoint != NULL);
    item = item->next;
  }
  CHECK(item->next == NULL);

  leela_naming_value_free(snapshot);
  leela_endpoint_free(endpoint);
}
