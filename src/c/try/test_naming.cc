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
#include "helpers.hh"
#include "leela/naming.h"
#include "leela/string.h"
#include "leela/endpoint.h"

TEST(test_leela_naming)
{
  zhandle_t *zh = zookeeper_init("localhost:2181", NULL, 60000, NULL, NULL, 0);
  test_leela_zk_rmrf(zh, "/leela-dev");
  test_leela_zk_write(zh, "/leela-dev", NULL);
  test_leela_zk_write(zh, "/leela-dev/child-0", "tcp://localhost:8080;");
  test_leela_zk_write(zh, "/leela-dev/child-1", "tcp://localhost:8081;");
  test_leela_zk_write(zh, "/leela-dev/child-2", "tcp://localhost:8082;");
  zookeeper_close(zh);

  leela_endpoint_t *endpoint     = leela_endpoint_load("tcp://localhost:2181;");
  leela_naming_t *naming         = leela_naming_init(endpoint, "/leela-dev", 2);
  leela_naming_value_t *snapshot = leela_naming_query(naming);
  leela_naming_value_t *item     = snapshot;
  for (int k=0; k<3; k+=1)
  {
    CHECK(item != NULL);
    CHECK(item->endpoint != NULL);
    item = item->next;
  }
  CHECK(item == NULL);
  leela_naming_shutdown(naming);
  leela_endpoint_free(endpoint);
  leela_naming_value_free(snapshot);
}
