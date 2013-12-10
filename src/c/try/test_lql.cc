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
#include "leela/lql.h"
#include "leela/naming.h"
#include "leela/string.h"
#include "leela/endpoint.h"

TEST(test_leela_without_backends)
{
  zhandle_t *zh = zookeeper_init("localhost:2181", NULL, 60000, NULL, NULL, 0);
  test_leela_zk_rmrf(zh, "/leela-dev");
  zookeeper_close(zh);

  leela_endpoint_t *endpoint = leela_endpoint_load("tcp://localhost:2181/leela-dev;");
  lql_context_t *context     = leela_lql_context_init(endpoint, "/naming/warpdrive");
  lql_cursor_t *cursor       = leela_lql_cursor_init(context, "dgvncsz0f", "secret", 1000);
  CHECK(cursor == NULL);

  leela_lql_context_close(context);
  leela_endpoint_free(endpoint);
}

TEST(test_leela_make_elem)
{
  zhandle_t *zh = zookeeper_init("localhost:2181", NULL, 60000, NULL, NULL, 0);
  test_leela_zk_rmrf(zh, "/leela-dev");
  test_leela_zk_write(zh, "/leela-dev", NULL);
  test_leela_zk_write(zh, "/leela-dev/naming", NULL);
  test_leela_zk_write(zh, "/leela-dev/naming/warpdrive", NULL);
  test_leela_zk_write(zh, "/leela-dev/naming/warpdrive/1", std::getenv("LEELA_ENDPOINT"));
  zookeeper_close(zh);

  leela_endpoint_t *endpoint = leela_endpoint_load("tcp://localhost:2181/leela-dev;");
  lql_context_t *context     = leela_lql_context_init(endpoint, "/naming/warpdrive");
  lql_cursor_t *cursor       = leela_lql_cursor_init(context, "dgvncsz0f", "", 1000);
  CHECK(cursor != NULL);

  leela_status rc = leela_lql_cursor_execute(cursor, "using (testing) make (leela);");
  CHECK(rc == LEELA_OK);

  CHECK_EQUAL(LEELA_OK, leela_lql_cursor_close(cursor));
  CHECK_EQUAL(LEELA_OK, leela_lql_context_close(context));
  leela_endpoint_free(endpoint);
}

TEST(test_leela_resolve_name)
{
  zhandle_t *zh = zookeeper_init("localhost:2181", NULL, 60000, NULL, NULL, 0);
  test_leela_zk_rmrf(zh, "/leela-dev");
  test_leela_zk_write(zh, "/leela-dev", NULL);
  test_leela_zk_write(zh, "/leela-dev/naming", NULL);
  test_leela_zk_write(zh, "/leela-dev/naming/warpdrive", NULL);
  test_leela_zk_write(zh, "/leela-dev/naming/warpdrive/1", std::getenv("LEELA_ENDPOINT"));
  zookeeper_close(zh);

  leela_endpoint_t *endpoint = leela_endpoint_load("tcp://localhost:2181/leela-dev;");
  lql_context_t *context     = leela_lql_context_init(endpoint, "/naming/warpdrive");
  lql_cursor_t *cursor;

  cursor = leela_lql_cursor_init(context, "dgvncsz0f", "", 1000);
  CHECK_EQUAL(LEELA_OK, leela_lql_cursor_execute(cursor, "using (testing) make (leela);"));
  CHECK_EQUAL(LEELA_EOF, leela_lql_cursor_next(cursor));
  CHECK_EQUAL(LEELA_OK, leela_lql_cursor_close(cursor));

  cursor = leela_lql_cursor_init(context, "dgvncsz0f", "", 1000);
  CHECK_EQUAL(LEELA_OK, leela_lql_cursor_execute(cursor, "using (testing) name 0x2242b7b65b677c562ec89a989ed132e384e01a7b0d1241246ea891e6;"));
  CHECK_EQUAL(LEELA_OK, leela_lql_cursor_next(cursor));
  CHECK_EQUAL(LQL_NAME, leela_lql_msg_type(cursor));
  lql_name_t *name = leela_lql_msg_name(cursor);
  CHECK(name != NULL);
  CHECK_EQUAL("dgvncsz0f", name->user);
  CHECK_EQUAL("testing", name->tree);
  CHECK_EQUAL("leela", name->name);
  leela_lql_msg_name_free(name);
  CHECK_EQUAL(LEELA_OK, leela_lql_cursor_close(cursor));

  CHECK_EQUAL(LEELA_OK, leela_lql_context_close(context));
  leela_endpoint_free(endpoint);
}
