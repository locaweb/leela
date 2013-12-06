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
#include "UnitTest++.h"
#include "leela/endpoint.h"

TEST(test_leela_endpoint__load_on_error_must_return_null)
{
  CHECK(leela_endpoint_load("foobar") == NULL);
}

TEST(test_leela_endpoint__load_with_single_hosts)
{
  leela_endpoint_t *endpoint = leela_endpoint_load("tcp://foobar:4080;");
  CHECK(endpoint != NULL);
  CHECK(endpoint->addrlen == 1);
  CHECK(endpoint->addrs != NULL);
  CHECK(endpoint->addrs[0].port == 4080);
  CHECK_EQUAL("foobar", endpoint->addrs[0].host);
  leela_endpoint_free(endpoint);
}

TEST(test_leela_endpoint__load_with_multiple_hosts)
{
  leela_endpoint_t *endpoint = leela_endpoint_load("tcp://foobar:4080,foobar:4081,foobar:4082;");
  CHECK(endpoint != NULL);
  CHECK(endpoint->addrlen == 3);
  CHECK(endpoint->addrs != NULL);
  CHECK(endpoint->addrs[0].port == 4080);
  CHECK(endpoint->addrs[1].port == 4081);
  CHECK(endpoint->addrs[2].port == 4082);
  CHECK_EQUAL("foobar", endpoint->addrs[0].host);
  CHECK_EQUAL("foobar", endpoint->addrs[1].host);
  CHECK_EQUAL("foobar", endpoint->addrs[2].host);
  leela_endpoint_free(endpoint);
}

TEST(test_leela_endpoint__load_dump_identity)
{
  char *dump;
  leela_endpoint_t *endpoint;

  endpoint = leela_endpoint_load("tcp://foobar:4080;");
  dump     = leela_endpoint_dump(endpoint);
  CHECK_EQUAL("tcp://foobar:4080;", dump);
  std::free(dump);
  leela_endpoint_free(endpoint);

  endpoint = leela_endpoint_load("tcp://foobar:4080,foobar:4081,foobar:4082/foobar;");
  dump     = leela_endpoint_dump(endpoint);
  CHECK_EQUAL("tcp://foobar:4080,foobar:4081,foobar:4082/foobar;", dump);
  std::free(dump);
  leela_endpoint_free(endpoint);
}

TEST(test_leela_endpoint__dup)
{
  leela_endpoint_t *endpoint_a = leela_endpoint_load("tcp://foobar:4080;");
  leela_endpoint_t *endpoint_b = leela_endpoint_dup(endpoint_a);
  char *dump_a                 = leela_endpoint_dump(endpoint_a);
  char *dump_b                 = leela_endpoint_dump(endpoint_b);
  CHECK_EQUAL(dump_a, dump_b);
  free(dump_b);
  free(dump_a);
  leela_endpoint_free(endpoint_b);
  leela_endpoint_free(endpoint_a);
}
