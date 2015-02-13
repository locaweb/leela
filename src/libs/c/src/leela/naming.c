/* Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
 *  
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <math.h>
#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <pthread.h>
#include "lql.h"
#include "string.h"
#include "naming.h"
#include "signature.h"

#if defined(HAS_SYM_WINSLEEP)
#  include <windows.h>
#  define SLEEP__(x) Sleep(x)
#elif defined(HAS_SYM_USLEEP)
#  include <unistd.h>
#  define SLEEP__(x) usleep(x * 1000)
#elif defined(HAS_SYM_SLEEP)
#  include <unistd.h>
#  define SLEEP__(x) sleep((unsigned int) x/1000)
#else
#  error "naming.c: no sleep function found"
#endif

struct leela_naming_t
{
  int                     maxdelay;
  int                     attempt;
  bool                    cancel;
  size_t                  lb_index;
  pthread_t               thread;
  pthread_cond_t         *notify;
  pthread_mutex_t         mutex;
  lql_context_t          *context;
  leela_naming_cluster_t *cluster0;
  leela_naming_cluster_t *cluster;
};

static
leela_naming_cluster_t *cluster_init__ (size_t size)
{
  size_t k;
  leela_naming_cluster_t *cluster;
  if (size == 0)
  { return(NULL); }
  cluster = (leela_naming_cluster_t *) malloc(sizeof(leela_naming_cluster_t));
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
      for (k=0; k<cluster->size; k+=1)
      { cluster->endpoint[k] = NULL; }
    }
  }
  return(cluster);
}

static
void naming_notify__ (leela_naming_t *naming)
{
  if (naming->notify != NULL)
  { pthread_cond_signal(naming->notify); }
  naming->notify = NULL;
}

static
leela_naming_cluster_t *naming_discover2__ (leela_naming_t *naming, const leela_endpoint_t *endpoint)
{
  int k;
  lql_tuple2_t *entry;
  lql_fail_t *failmsg            = NULL;
  lql_cursor_t *cursor           = leela_lql_cursor_init_on(naming->context, endpoint, NULL, NULL, 1000 * powl(2, naming->attempt));
  lql_stat_t *stat               = NULL;
  size_t count                   = 0;
  leela_naming_cluster_t *result = NULL;

  if (cursor == NULL)
  { return(NULL); }
  if (leela_lql_cursor_execute(cursor, "using (system) stat;") != LEELA_OK)
  { goto handle_error; }
  if (leela_lql_cursor_next(cursor) != LEELA_OK)
  { goto handle_error; }
  if (leela_lql_fetch_type(cursor) != LQL_STAT_MSG)
  {
    if (leela_lql_fetch_type(cursor) == LQL_FAIL_MSG)
    { failmsg = leela_lql_fetch_fail(cursor); }
    goto handle_error;
  }

  stat = leela_lql_fetch_stat(cursor);
  if (stat == NULL || stat->size == 0)
  { goto handle_error; }

  for (k=0; k<stat->size; k+=1)
  {
    entry = (stat->attrs + k);
    if (strcmp((char *) entry->fst, "endpoint/warpdrive") == 0)
    { count += 1; }
  }

  result = cluster_init__(count);
  if (result == NULL)
  { goto handle_error; }

  count = 0;
  for (k=0; k<stat->size; k+=1)
  {
    entry = (stat->attrs + k);
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

  naming->attempt = LEELA_MAX(1, naming->attempt - 1);
  leela_lql_stat_free(stat);
  leela_lql_cursor_close(cursor);
  return(result);

handle_error:
  naming->attempt = LEELA_MIN(naming->attempt + 1, 8);
  if (failmsg != NULL)
  { LEELA_DEBUG2(naming->context, "leela_fail: [%d] %s", failmsg->code, failmsg->message); }
  leela_lql_fail_free(failmsg);
  leela_naming_cluster_free(result);
  leela_lql_stat_free(stat);
  leela_lql_cursor_close(cursor);
  return(NULL);
}

static
leela_naming_cluster_t *naming_discover__ (leela_naming_t *naming, const leela_naming_cluster_t *cluster)
{
  long int rnd = 0;
  size_t k, offset;
  leela_naming_cluster_t *new_cluster = NULL;
  if (cluster != NULL)
  {
    if (leela_random_next(leela_random(naming->context), &rnd) != 0
        && leela_random_read(leela_random(naming->context), &rnd, sizeof(rnd)) != 0)
    { LEELA_DEBUG0(naming->context, "naming_discover: error reading random number"); }
    offset = (size_t) rnd;
    for (k=0; k<cluster->size; k+=1)
    {
      new_cluster = naming_discover2__(naming, cluster->endpoint[(k + offset) % cluster->size]);
      if (new_cluster != NULL)
      { break; }
    }
  }
  LEELA_DEBUG2(naming->context,
              "naming_discover: cur: %d, new: %d",
              (cluster == NULL ? 0 : cluster->size),
              (new_cluster == NULL ? 0 : new_cluster->size));
  return(new_cluster);
}

static
void *naming_loop__ (void *data)
{
  unsigned int w_wait;
  leela_naming_cluster_t *cluster = NULL;
  leela_naming_t *naming          = (leela_naming_t *) data;
  LEELA_DEBUG0(naming->context, "ENTER:naming loop");
  do
  {
    if (naming->context != NULL)
    {
      if (naming->cluster != NULL)
      { cluster = naming_discover__(naming, naming->cluster); }
      else
      { cluster = naming_discover__(naming, naming->cluster0); }
    }

    if (pthread_mutex_lock(&naming->mutex) == 0)
    {
      leela_naming_cluster_free(naming->cluster);
      naming->cluster = cluster;
      pthread_mutex_unlock(&naming->mutex);
    }
    naming_notify__(naming);

    if (naming->context == NULL || naming->cluster == NULL)
    { w_wait = 250; }
    else
    { w_wait = (30 + (((unsigned int) rand()) % naming->maxdelay)) * 1000; }
    LEELA_DEBUG1(naming->context, "naming: waiting %d ms before next query", w_wait);
    while (!naming->cancel && w_wait > 0)
    {
      if (w_wait >= 1000)
      {
        SLEEP__(1000);
        w_wait -= 1000;
      }
      else
      {
        SLEEP__(w_wait);
        w_wait = 0;
      }
    }
  } while (! naming->cancel);
  LEELA_DEBUG0(naming->context, "EXIT:naming loop");
  return(NULL);
}

bool leela_naming_start (leela_naming_t *naming, lql_context_t *context)
{
  bool ok = false;
  pthread_mutex_t mutex;
  pthread_cond_t notify;
  if (pthread_mutex_init(&mutex, NULL) != 0)
  { return(false); }

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
  { LEELA_DEBUG0(naming->context, "could not acquire exclusive lock!"); }

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
  long int rnd;
  leela_endpoint_t *endpoint = NULL;
  if (pthread_mutex_lock(&naming->mutex) == 0)
  {
    if (naming->cluster != NULL && naming->cluster->size > 0)
    {
      if (leela_random_next(leela_random(naming->context), &rnd) == 0
          || leela_random_read(leela_random(naming->context), &rnd, sizeof(rnd)) == 0)
      { endpoint = leela_endpoint_dup(naming->cluster->endpoint[rnd % naming->cluster->size]); }
      else
      { LEELA_DEBUG0(naming->context, "leela_naming_select: error reading next random number"); }
    }
    pthread_mutex_unlock(&naming->mutex);
  }
  return(endpoint);
}

leela_naming_t *leela_naming_init (const leela_endpoint_t *const *warpdrive, int maxdelay)
{
  size_t k;
  leela_naming_t *naming                  = (leela_naming_t *) malloc(sizeof(leela_naming_t));
  const leela_endpoint_t *const *iterator = warpdrive;
  if (naming == NULL)
  { return(NULL); }
  naming->maxdelay = LEELA_MIN(60, LEELA_MAX(5, maxdelay));
  naming->cancel   = false;
  naming->attempt  = 1;
  naming->cluster  = NULL;
  naming->cluster0 = NULL;
  naming->context  = NULL;
  naming->notify   = NULL;

  k = 0;
  while (iterator[k] != NULL)
  { k += 1; }
  naming->cluster  = cluster_init__(k);
  naming->cluster0 = cluster_init__(k);
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

  pthread_create(&naming->thread, NULL, naming_loop__, naming);
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
  size_t k;
  if (cluster != NULL)
  {
    for (k=0; k<cluster->size; k+=1)
    { leela_endpoint_free(cluster->endpoint[k]); }
    free(cluster->endpoint);
    free(cluster);
  }
}

leela_naming_cluster_t *leela_naming_discover (leela_naming_t *naming)
{
  size_t k;
  leela_naming_cluster_t *cluster = NULL;
  if (pthread_mutex_lock(&naming->mutex) != 0)
  {
    LEELA_DEBUG0(naming->context, "could not acquire exclusive lock!");
    return(NULL);
  }

  if (naming->cluster != NULL)
  {
    cluster = cluster_init__(naming->cluster->size);
    if (cluster != NULL)
    {
      for (k=0; k<naming->cluster->size; k+=1)
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
