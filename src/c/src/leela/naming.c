// Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
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
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <pthread.h>
#include <sys/select.h>
#include "leela/debug.h"
#include "leela/string.h"
#include "leela/naming.h"

struct leela_naming_t
{
  int                     maxdelay;
  bool                    cancel;
  pthread_t               thread;
  pthread_cond_t         *notify;
  pthread_mutex_t         mutex;
  lql_context_t          *context;
  leela_naming_cluster_t *cluster0;
  leela_naming_cluster_t *cluster;
};

static
leela_naming_cluster_t *__cluster_init (size_t size)
{
  if (size == 0)
  { return(NULL); }
  leela_naming_cluster_t *cluster = (leela_naming_cluster_t *) malloc(sizeof(leela_naming_cluster_t));
  if (cluster != NULL)
  {
    cluster->size     = size;
    cluster->endpoint = (leela_endpoint_t **) malloc(size * sizeof(leela_endpoint_t *));
    if (cluster->endpoint == NULL)
    {
      free(cluster);
      cluster = NULL;
    }
    else
    {
      for (size_t k=0; k<cluster->size; k+=1)
      { cluster->endpoint[k] = NULL; }
    }
  }
  return(cluster);
}

static
void __naming_notify (leela_naming_t *naming)
{
  if (naming->notify != NULL)
  { pthread_cond_signal(naming->notify); }
  naming->notify = NULL;
}

static
leela_naming_cluster_t *__naming_discover2 (leela_naming_t *naming, const leela_endpoint_t *endpoint)
{
  lql_cursor_t *cursor           = leela_lql_cursor_init2(naming->context, endpoint, "nobody", "nobody", 1000);
  lql_stat_t *stat               = NULL;
  size_t count                   = 0;
  leela_naming_cluster_t *result = NULL;

  if (cursor == NULL)
  { return(NULL); }
  if (leela_lql_cursor_execute(cursor, "using (system) stat;") != LEELA_OK)
  { goto handle_error; }
  if (leela_lql_cursor_next(cursor) != LEELA_OK || leela_lql_fetch_type(cursor) != LQL_STAT_MSG)
  { goto handle_error; }

  stat = leela_lql_fetch_stat(cursor);
  if (stat == NULL || stat->size == 0)
  { goto handle_error; }

  for (int k=0; k<stat->size; k+=1)
  {
    lql_tuple2_t *entry = (stat->attrs + k);
    if (strcmp((char *) entry->fst, "endpoint/warpdrive") == 0)
    { count += 1; }
  }

  result = __cluster_init(count);
  if (result == NULL)
  { goto handle_error; }

  count = 0;
  for (int k=0; k<stat->size; k+=1)
  {
    lql_tuple2_t *entry = (stat->attrs + k);
    if (strcmp((char *) entry->fst, "endpoint/warpdrive") == 0)
    {
      result->endpoint[count] = leela_endpoint_load((char *) entry->snd);
      if (result->endpoint[count] == NULL)
      { goto handle_error; }
      count += 1;
    }
  }

  if (count == 0)
  { goto handle_error; }

  leela_lql_stat_free(stat);
  leela_lql_cursor_close(cursor);
  return(result);

handle_error:
  leela_naming_cluster_free(result);
  leela_lql_stat_free(stat);
  leela_lql_cursor_close(cursor);
  return(NULL);
}

static
leela_naming_cluster_t *__naming_discover (leela_naming_t *naming, const leela_naming_cluster_t *cluster)
{
  leela_naming_cluster_t *new_cluster = NULL;
  if (cluster != NULL)
  {
    size_t offset = ((size_t) lrand48()) - cluster->size;
    for (size_t k=0; k<cluster->size; k+=1)
    {
      new_cluster = __naming_discover2(naming, cluster->endpoint[(k + offset) % cluster->size]);
      if (new_cluster != NULL)
      { break; }
    }
  }
  LEELA_DEBUG("naming_discover: cur: %d, new: %d",
              (cluster == NULL ? 0 : cluster->size),
              (new_cluster == NULL ? 0 : new_cluster->size));
  return(new_cluster);
}

static
void *__naming_loop (void *data)
{
  LEELA_DEBUG0("ENTER:naming loop");
  leela_naming_t *naming = (leela_naming_t *) data;
  do
  {
    leela_naming_cluster_t *cluster = NULL;
    if (naming->context != NULL)
    { cluster = __naming_discover(naming, naming->cluster); }
    if (cluster == NULL)
    { cluster = __naming_discover(naming, naming->cluster0); }

    if (pthread_mutex_lock(&naming->mutex) == 0)
    {
      leela_naming_cluster_free(naming->cluster);
      naming->cluster = cluster;
      pthread_mutex_unlock(&naming->mutex);
    }
    __naming_notify(naming);

    int w_sec, w_usec;
    if (naming->context == NULL || naming->cluster == NULL)
    {
      w_sec  = 0;
      w_usec = 250000;
    }
    else
    {
      w_sec  = ((unsigned int) rand()) % naming->maxdelay;
      w_usec = 0;
    }
    while (!naming->cancel && (w_sec > 0 || w_usec > 0))
    {
      struct timeval tv;
      tv.tv_sec  = (w_sec > 0 ? 1 : 0);
      tv.tv_usec = (w_usec > 0 ? w_usec : 0);
      select(0, NULL, NULL, NULL, &tv);
      w_sec     -= 1;
      w_usec     = 0;
    }
  } while (! naming->cancel);
  LEELA_DEBUG0("EXIT:naming loop");
  return(NULL);
}

bool leela_naming_start (leela_naming_t *naming, lql_context_t *context)
{
  bool ok = false;
  pthread_mutex_t mutex;
  if (pthread_mutex_init(&mutex, NULL) != 0)
  { return(false); }

  pthread_cond_t notify;
  if (pthread_cond_init(&notify, NULL) != 0)
  {
    pthread_mutex_destroy(&mutex);
    return(false);
  }

  naming->notify  = &notify;
  naming->context = context;

  if (pthread_mutex_lock(&mutex) == 0)
  {
    pthread_cond_wait(&notify, &mutex);
    pthread_mutex_unlock(&mutex);
  }
  else
  { LEELA_DEBUG0("could not acquire exclusive lock!"); }

  if (pthread_mutex_lock(&naming->mutex) == 0)
  {
    ok = naming->cluster != NULL;
    pthread_mutex_unlock(&naming->mutex);
  }

  pthread_cond_destroy(&notify);
  pthread_mutex_destroy(&mutex);

  return(ok);
}

leela_endpoint_t *leela_naming_select (leela_naming_t *naming)
{
  leela_endpoint_t *endpoint = NULL;
  if (pthread_mutex_lock(&naming->mutex) == 0)
  {
    if (naming->cluster->size > 0)
    {
      unsigned int at = lrand48() % naming->cluster->size;
      endpoint        = leela_endpoint_dup(naming->cluster->endpoint[at]);
    }
    pthread_mutex_unlock(&naming->mutex);
  }
  return(endpoint);
}

leela_naming_t *leela_naming_init (const leela_endpoint_t *const *warpdrive, int maxdelay)
{
  leela_naming_t *naming = (leela_naming_t *) malloc(sizeof(leela_naming_t));
  if (naming == NULL)
  { return(NULL); }
  naming->maxdelay = maxdelay;
  naming->cancel   = false;
  naming->cluster  = NULL;
  naming->cluster0 = NULL;
  naming->context  = NULL;
  naming->notify   = NULL;

  const leela_endpoint_t *const *iterator = warpdrive;
  size_t k = 0;
  while (iterator[k] != NULL)
  { k += 1; }
  naming->cluster  = __cluster_init(k);
  naming->cluster0 = __cluster_init(k);
  if (naming->cluster == NULL || naming->cluster0 == NULL)
  { goto handle_error; }
  for (k=0; warpdrive[k]!=NULL; k+=1)
  {
    naming->cluster->endpoint[k]  = leela_endpoint_dup(warpdrive[k]);
    naming->cluster0->endpoint[k] = leela_endpoint_dup(warpdrive[k]);
    if (naming->cluster->endpoint[k] == NULL || naming->cluster0->endpoint[k] == NULL)
    { goto handle_error; }
  }

  if (pthread_mutex_init(&naming->mutex, NULL) != 0)
  { goto handle_error; }

  pthread_create(&naming->thread, NULL, __naming_loop, naming);
  return(naming);

handle_error:
  leela_naming_cluster_free(naming->cluster);
  leela_naming_cluster_free(naming->cluster0);
  free(naming);
  return(NULL);
}

void leela_naming_destroy (leela_naming_t *naming)
{
  naming->cancel = true;
  pthread_join(naming->thread, NULL);
  leela_naming_cluster_free(naming->cluster);
  leela_naming_cluster_free(naming->cluster0);
  pthread_mutex_destroy(&naming->mutex);
  free(naming);
}

void leela_naming_cluster_free (leela_naming_cluster_t *cluster)
{
  if (cluster != NULL)
  {
    for (size_t k=0; k<cluster->size; k+=1)
    { leela_endpoint_free(cluster->endpoint[k]); }
    free(cluster->endpoint);
    free(cluster);
  }
}

leela_naming_cluster_t *leela_naming_discover (leela_naming_t *naming)
{
  if (pthread_mutex_lock(&naming->mutex) != 0)
  {
    LEELA_DEBUG0("could not acquire exclusive lock!");
    return(NULL);
  }

  leela_naming_cluster_t *cluster = NULL;
  if (naming->cluster != NULL)
  {
    cluster = __cluster_init(naming->cluster->size);
    if (cluster != NULL)
    {
      for (size_t k=0; k<naming->cluster->size; k+=1)
      {
        cluster->endpoint[k] = leela_endpoint_dup(naming->cluster->endpoint[k]);
        if (cluster->endpoint[k] == NULL)
        {
          leela_naming_cluster_free(cluster);
          cluster = NULL;
          break;
        }
      }
    }
  }
  pthread_mutex_unlock(&naming->mutex);
  return(cluster);
}
