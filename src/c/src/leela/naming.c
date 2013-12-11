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

struct leela_naming_t
{
  char                   *resource;
  int                     maxdelay;
  char                   *endpoint;
  bool                    cancel;
  pthread_t               thread;
  pthread_cond_t         *notify;
  pthread_mutex_t         mutex;
  leela_naming_value_t *state;
};

static
leela_naming_value_t *__value_init()
{
  leela_naming_value_t *value = (leela_naming_value_t *) malloc(sizeof(leela_naming_value_t));
  if (value != NULL)
  {
    value->next     = NULL;
    value->endpoint = NULL;
  }
  return(value);
}

static
leela_naming_value_t *__zk_dump_tree(zhandle_t *zh, leela_naming_value_t *result, const char *path, char *buffer, int bufflen0)
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
void __naming_notify(leela_naming_t *naming)
{
  if (naming->notify != NULL)
  { pthread_cond_signal(naming->notify); }
  naming->notify = NULL;
}

static
void *__naming_loop(void *data)
{
  srand(time(NULL));
  int nextin             = 0;
  zhandle_t *zh          = NULL;
  leela_naming_t *naming = (leela_naming_t *) data;
  do
  {
    if (zh != NULL)
    { zookeeper_close(zh); }

    LEELA_DEBUG("naming thread: %s sleeping %d seconds", naming->resource, nextin);
    for (; nextin > 0 && !naming->cancel; nextin -= 1)
    { sleep(1); }
    if (naming->cancel)
    { break; }
    nextin = rand() % naming->maxdelay;

    zh = zookeeper_init(naming->endpoint + 6, NULL, 60000, NULL, NULL, 0);
    if (zh == NULL)
    {
      LEELA_DEBUG("could not connect to zookeeper: %s => %s", naming->resource, naming->endpoint);
      __naming_notify(naming);
      nextin = 1;
      continue;
    }

    LEELA_DEBUG("fetching data from zookeeper: %s => %s", naming->resource, naming->endpoint);
    leela_naming_value_t *state = __value_init();
    int bufflen                 = 1024 * 1024;
    char *buffer                = (char *) malloc(bufflen);
    leela_naming_value_t *rc    = __zk_dump_tree(zh, state, naming->resource, buffer, bufflen);
    free(buffer);
    if (rc == NULL)
    {
      LEELA_DEBUG("error reading from zookeeper: %s => %s", naming->resource, naming->endpoint);
      leela_naming_value_free(state);
      __naming_notify(naming);
      nextin = 1;
      continue;
    }
    if (pthread_mutex_lock(&naming->mutex) == 0)
    {
      leela_naming_value_free(naming->state);
      naming->state = state;
      pthread_mutex_unlock(&naming->mutex);
    }
    __naming_notify(naming);
  } while (true);

  LEELA_DEBUG("terminating naming thread: %s", naming->resource);
  return(NULL);
}

leela_naming_t *leela_naming_start(const leela_endpoint_t *endpoint, const char *resource, int maxdelay)
{
  pthread_cond_t notify;
  if (pthread_cond_init(&notify, NULL) != 0)
  { return(NULL); }

  leela_naming_t *naming = (leela_naming_t *) malloc(sizeof(leela_naming_t));
  if (naming == NULL)
  { return(NULL); }
  naming->maxdelay = maxdelay;
  naming->resource = NULL;
  naming->endpoint = NULL;
  naming->state    = NULL;
  naming->cancel   = false;
  naming->notify   = &notify;

  naming->endpoint = leela_endpoint_dump(endpoint);
  if (naming->endpoint == NULL)
  { return(NULL); }
  naming->endpoint[strlen(naming->endpoint) - 1] = '\0';

  naming->resource = leela_strdup(resource);
  if (naming->resource == NULL)
  { goto handle_error; }

  if (pthread_mutex_init(&naming->mutex, NULL) != 0)
  { goto handle_error; }

  pthread_create(&naming->thread, NULL, __naming_loop, naming);
  if (pthread_mutex_lock(&naming->mutex) == 0)
  {
    pthread_cond_wait(&notify, &naming->mutex);
    pthread_mutex_unlock(&naming->mutex);
  }
  pthread_cond_destroy(&notify);
  return(naming);

handle_error:
  free(naming->resource);
  free(naming->endpoint);
  free(naming);
  return(NULL);
}

void leela_naming_stop(leela_naming_t *naming)
{
  naming->cancel = true;
  pthread_join(naming->thread, NULL);
  free(naming->resource);
  free(naming->endpoint);
  leela_naming_value_free(naming->state);
  pthread_mutex_destroy(&naming->mutex);
  free(naming);
}

void leela_naming_value_free(leela_naming_value_t *value)
{
  while (value != NULL)
  {
    leela_naming_value_t *tmp = value;
    leela_endpoint_free(tmp->endpoint);
    value = tmp->next;
    free(tmp);
  }
}

leela_naming_value_t *leela_naming_query(leela_naming_t *naming)
{
  if (pthread_mutex_lock(&naming->mutex) != 0)
  { return(NULL); }

  leela_naming_value_t *value = NULL;
  if (naming->state != NULL)
  {
    leela_naming_value_t *llist = NULL;
    leela_naming_value_t *state = naming->state;
    while (state != NULL)
    {
      if (state->endpoint != NULL)
      {
        if (llist == NULL)
        {
          llist = __value_init();
          value = llist;
        }
        else
        {
          llist->next = __value_init();
          llist       = llist->next;
        }
        if (llist != NULL)
        { llist->endpoint = leela_endpoint_dup(state->endpoint); }
        if (llist == NULL || llist->endpoint == NULL)
        {
          leela_naming_value_free(value);
          value = NULL;
          break;
        }
      }
      state = state->next;
    };
  }
  pthread_mutex_unlock(&naming->mutex);
  return(value);
}
