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
#include "UnitTest++.h"
#include "leela/endpoint.h"

TEST(test_leela_endpoint__load_on_error_must_return_null)
{
  CHECK(leela_endpoint_load("foobar") == NULL);
}

TEST(test_leela_endpoint__load_with_single_hosts)
{
  leela_endpoint_t *endpoint = leela_endpoint_load("tcp://foobar:4080");
  CHECK(endpoint != NULL);
  CHECK_EQUAL(4080, endpoint->port);
  CHECK_EQUAL("foobar", endpoint->host);
  leela_endpoint_free(endpoint);
}

TEST(test_leela_endpoint__load_dump_identity)
{
  char *dump;
  leela_endpoint_t *endpoint;

  endpoint = leela_endpoint_load("tcp://foobar:4080");
  dump     = leela_endpoint_dump(endpoint);
  CHECK_EQUAL("tcp://foobar:4080", dump);
  std::free(dump);
  leela_endpoint_free(endpoint);
}

TEST(test_leela_endpoint__dup)
{
  leela_endpoint_t *endpoint_a = leela_endpoint_load("tcp://foobar:4080");
  leela_endpoint_t *endpoint_b = leela_endpoint_dup(endpoint_a);
  char *dump_a                 = leela_endpoint_dump(endpoint_a);
  char *dump_b                 = leela_endpoint_dump(endpoint_b);
  CHECK_EQUAL(dump_a, dump_b);
  free(dump_b);
  free(dump_a);
  leela_endpoint_free(endpoint_b);
  leela_endpoint_free(endpoint_a);
}
