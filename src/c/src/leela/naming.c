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

#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <pthread.h>
#include <zookeeper/zookeeper.h>
#include "leela/debug.h"
#include "leela/string.h"
#include "leela/naming.h"

struct leela_resolver_t
{
  char                   *resource;
  char                   *endpoint;
  bool                    cancel;
  pthread_t               thread;
  pthread_mutex_t         mutex;
  leela_resolver_value_t *state;
};

static
leela_resolver_value_t *__value_init()
{
  leela_resolver_value_t *value = (leela_resolver_value_t *) malloc(sizeof(leela_resolver_value_t));
  if (value != NULL)
  {
    value->next     = NULL;
    value->endpoint = NULL;
  }
  return(value);
}

static
leela_resolver_value_t *__zk_dump_tree(zhandle_t *zh, leela_resolver_value_t *result, const char *path, char *buffer, int bufflen0)
{
  int bufflen = bufflen0;
  int rc      = zoo_get(zh, path, 0, buffer, &bufflen, NULL);
  if (rc != ZOK)
  {
    LEELA_DEBUG("error reading path from zookeeper: %s [rc=%d]", path, rc);
    return(NULL);
  }

  int offset=0;
  for (int k=0; result!=NULL && k<=bufflen; k+=1)
  {
    if (k == bufflen || buffer[k] == '\n')
    {
      buffer[k] = '\0';
      result->endpoint = leela_endpoint_load(buffer + offset);
      if (result->endpoint != NULL)
      {
        LEELA_DEBUG("found endpoint: %s => %s", path, buffer+offset);
        result->next = __value_init();
        result       = result->next;
      }
      offset = k + 1;
    }
  }

  struct String_vector children;
  rc = zoo_get_children(zh, path, 0, &children);
  if (rc != ZOK)
  { return(NULL); }
  for (int32_t k=0; result != NULL && k<children.count; k+=1)
  {
    char *tmp = leela_join(path, "/", children.data[k], NULL);
    if (tmp == NULL)
    { return(NULL); }
    result = __zk_dump_tree(zh, result, tmp, buffer, bufflen0);
    free(tmp);
  }

  return(result);
}

static
void *__resolve_loop(void *data)
{
  srand(time(NULL));
  int nextin                 = 0;
  zhandle_t *zh              = NULL;
  leela_resolver_t *resolver = (leela_resolver_t *) data;
  do
  {
    if (zh != NULL)
    { zookeeper_close(zh); }

    sleep(nextin);
    nextin = 5 + rand() % 10;

    zh = zookeeper_init(resolver->endpoint + 6, NULL, 60000, NULL, NULL, 0);
    if (zh == NULL)
    {
      LEELA_DEBUG("could not connect to zookeeper: %s => %s", resolver->resource, resolver->endpoint);
      continue;
    }

    LEELA_DEBUG("fetching data from zookeeper: %s => %s", resolver->resource, resolver->endpoint);
    leela_resolver_value_t *state = __value_init();
    int bufflen                   = 1024 * 1024;
    char *buffer                  = (char *) malloc(bufflen);
    leela_resolver_value_t *rc    = __zk_dump_tree(zh, state, resolver->resource, buffer, bufflen);
    free(buffer);
    if (rc == NULL)
    {
      LEELA_DEBUG("error reading from zookeeper: %s => %s", resolver->resource, resolver->endpoint);
      leela_resolver_value_free(state);
      continue;
    }
    if (pthread_mutex_lock(&resolver->mutex) == 0)
    {
      leela_resolver_value_free(resolver->state);
      resolver->state = state;
      pthread_mutex_unlock(&resolver->mutex);
    }

    LEELA_DEBUG("naming thread: %s next-in %d seconds", resolver->resource, nextin);
  } while (! resolver->cancel);

  if (zh != NULL)
  { zookeeper_close(zh); }

  LEELA_DEBUG("terminating naming thread: %s", resolver->resource);

  return(NULL);
}

leela_resolver_t *leela_resolver_init(const leela_endpoint_t *endpoint, const char *resource)
{
  leela_resolver_t *resolver = (leela_resolver_t *) malloc(sizeof(leela_resolver_t));
  if (resolver == NULL)
  { return(NULL); }
  resolver->resource = NULL;
  resolver->endpoint = NULL;
  resolver->state    = NULL;
  resolver->cancel   = false;

  resolver->endpoint = leela_endpoint_dump(endpoint);
  if (resolver->endpoint == NULL)
  { return(NULL); }
  resolver->endpoint[strlen(resolver->endpoint) - 1] = '\0';

  resolver->resource = leela_strdup(resource);
  if (resolver->resource == NULL)
  { goto handle_error; }

  if (pthread_mutex_init(&resolver->mutex, NULL) != 0)
  { goto handle_error; }

  pthread_create(&resolver->thread, NULL, __resolve_loop, resolver);
  return(resolver);

handle_error:
  free(resolver->resource);
  free(resolver->endpoint);
  free(resolver);
  return(NULL);
}

void leela_resolver_shutdown(leela_resolver_t *resolver, leela_resolver_value_t **result)
{
  resolver->cancel = true;
  pthread_join(resolver->thread, NULL);
  free(resolver->resource);
  free(resolver->endpoint);
  if (result != NULL)
  { *result = resolver->state; }
  else
  { leela_resolver_value_free(resolver->state); }
  pthread_mutex_destroy(&resolver->mutex);
  free(resolver);
}

void leela_resolver_value_free(leela_resolver_value_t *value)
{
  while (value != NULL)
  {
    leela_resolver_value_t *tmp = value;
    leela_endpoint_free(tmp->endpoint);
    value = tmp->next;
    free(tmp);
  }
}

leela_resolver_value_t *leela_resolver_query(leela_resolver_t *resolver)
{
  if (pthread_mutex_lock(&resolver->mutex) != 0)
  { return(NULL); }

  leela_resolver_value_t *value = NULL;
  if (resolver->state != NULL)
  {
    value                         = __value_init();
    leela_resolver_value_t *tmp   = value;
    leela_resolver_value_t *state = resolver->state;
    while (state != NULL && tmp != NULL)
    {
      if (state->endpoint != NULL)
      {
        tmp->endpoint = leela_endpoint_dup(state->endpoint);
        tmp->next     = (state->next == NULL ? NULL : __value_init());
        tmp           = tmp->next;
      }
      state = state->next;
    };
  }
  pthread_mutex_unlock(&resolver->mutex);
  return(value);
}
