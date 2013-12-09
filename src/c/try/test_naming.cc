/* This file is part of Leela.
 *
 * Leela is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Leela is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Leela.  If not, see <http://www.gnu.org/licenses/>.
 */

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

  leela_endpoint_t *endpoint       = leela_endpoint_load("tcp://localhost:2181;");
  leela_resolver_t *resolver       = leela_resolver_init(endpoint, "/leela-dev");
  leela_resolver_value_t *snapshot = NULL;
  leela_resolver_shutdown(resolver, &snapshot);
  leela_resolver_value_t *item = snapshot;
  for (int k=0; k<3; k+=1)
  {
    CHECK(item != NULL);
    CHECK(item->endpoint != NULL);
    item = item->next;
  }
  CHECK(item->next == NULL);

  leela_resolver_value_free(snapshot);
  leela_endpoint_free(endpoint);
}
