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

#ifndef __write_leela_h__
#define __write_leela_h__

#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <strings.h>
#include <inttypes.h>
#include <sys/types.h>

#include <collectd/config.h>
#include <collectd/collectd.h>
#include <collectd/common.h>
#include <collectd/utils_cache.h>
#include <collectd/plugin.h>

#include <leela/lql.h>
#include <leela/endpoint.h>

#define WL_UNUSED(x) (void) x

typedef struct
{
  lql_context_t  *ctx;
  char           *user;
  char           *pass;
  char           *tree;
  char           *guid;
  int             timeout;
  cdtime_t        ctime;
  char           *sndbuf;
  size_t          sndbufoff;
  size_t          sndbuflen;
  pthread_mutex_t mutex;
} wl_data_t;

static
void wl_cluster_free (leela_endpoint_t **cluster)
{
  int k;
  if (cluster != NULL)
  {
    for (k=0; cluster != NULL && cluster[k] != NULL; k+=1)
    { leela_endpoint_free(cluster[k]); }
    sfree(cluster);
  }
}

static
char *wl_strdup (const char *src)
{
  size_t len = strlen(src);
  char *dst  = (char *) malloc(len + 1);
  if (dst != NULL)
  {
    strcpy(dst, src);
    dst[len] = '\0';
  }
  else
  { ERROR("write_leela plugin: malloc error!"); }
  return(dst);
}

static
int wl_blank (const char *s)
{ return(s == NULL || strcmp("", s) == 0); }

static
char *wl_unquote (char *dst, size_t maxlen, const char *src)
{
  size_t k, m = 0, escape = 0;
  size_t len  = strlen(src);
  for (k=0; k<len && m<maxlen-1; k+=1)
  {
    if (escape == 1)
    { escape = 0; }
    else if (src[k] == '\\')
    { escape = 1; }
    else if (src[k] == '"')
    { continue; }

    dst[m++] = src[k];
  }
  dst[m] = '\0';
  return(dst);
}

static
void wl_reset_buffer (wl_data_t *cfg)
{
  cfg->ctime     = cdtime();
  cfg->sndbufoff = 0;
}

static
int wl_send (wl_data_t *cfg)
{
  int rc = -1;
  lql_cursor_t *cursor;

  INFO("write_leela plugin: wl_send(size:%zu)", cfg->sndbufoff);
  if (cfg->sndbufoff == 0)
  { return(0); }

  cursor = leela_lql_cursor_init(cfg->ctx, cfg->user, cfg->pass, cfg->timeout);
  if (cursor == NULL)
  {
    ERROR("write_leela plugin: error initializing cursor");
    return(-1);
  }

  cfg->sndbuf[cfg->sndbufoff-2] = ';';
  cfg->sndbuf[cfg->sndbufoff-1] = '\0';
  if (leela_lql_cursor_execute(cursor, cfg->sndbuf) == LEELA_OK
      && leela_lql_cursor_next(cursor) == LEELA_EOF)
  { rc = 0; }
  leela_lql_cursor_close(cursor);

  if (rc != 0)
  { ERROR("write_leela plugin: error sending data to leela"); }
  else
  { wl_reset_buffer(cfg); }
  return(rc);
}

static
void wl_data_free (void *ptr)
{
  wl_data_t *data = (wl_data_t *) ptr;
  if (ptr != NULL)
  {
    if (pthread_mutex_lock(&data->mutex) == 0)
    {
      wl_send(data);
      leela_lql_context_close(data->ctx);
      sfree(data->user);
      sfree(data->pass);
      sfree(data->tree);
      sfree(data->guid);
      sfree(data->sndbuf);

      pthread_mutex_unlock(&data->mutex);
      pthread_mutex_destroy(&data->mutex);

      sfree(data);
    }
  }
}

static
int wl_print (wl_data_t *cfg, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  size_t avail = cfg->sndbuflen - cfg->sndbufoff;
  int rc       = vsnprintf(cfg->sndbuf + cfg->sndbufoff, avail, fmt, ap);
  if (rc > 0 && ((size_t) rc) < avail)
  {
    cfg->sndbufoff += rc;
    rc = 0;
  }
  va_end(ap);

  return(rc);
}

static
int wl_print_name (wl_data_t *cfg, const value_list_t *vl, const char *type)
{
  char v_host[DATA_MAX_NAME_LEN];
  char v_plugin[DATA_MAX_NAME_LEN];
  char v_plugin_instance[DATA_MAX_NAME_LEN];
  char v_type[DATA_MAX_NAME_LEN];
  char v_type_instance[DATA_MAX_NAME_LEN];

  return(wl_print(cfg,
                  "%s/%s%s%s/%s%s%s%s%s",
                  wl_unquote(v_host, DATA_MAX_NAME_LEN, vl->host),
                  wl_unquote(v_plugin, DATA_MAX_NAME_LEN, vl->plugin),
                  (wl_blank(vl->plugin_instance) ? "" : "-"),
                  (wl_blank(vl->plugin_instance) ? "" : wl_unquote(v_plugin_instance, DATA_MAX_NAME_LEN, vl->plugin_instance)),
                  wl_unquote(v_type, DATA_MAX_NAME_LEN, vl->type),
                  (wl_blank(vl->type_instance) ? "" : "-"),
                  (wl_blank(vl->type_instance) ? "" : wl_unquote(v_type_instance, DATA_MAX_NAME_LEN, vl->type_instance)),
                  (type == NULL ? "" : "/"),
                  (type == NULL ? "" : type)));
}

static
int wl_print_time (wl_data_t *cfg, const cdtime_t cdtime)
{
  struct timespec time;
  CDTIME_T_TO_TIMESPEC(cdtime, &time);
  return(wl_print(cfg, "%ld.%ld", (long) time.tv_sec, time.tv_nsec));
}

// TODO:use something better than %g
static
int wl_print_value (wl_data_t *cfg, int type, const value_t *value)
{
  int rc = 0;
  if (type == DS_TYPE_GAUGE)
  { rc = wl_print(cfg, "(double %g)", value->gauge); }
  else if (type == DS_TYPE_COUNTER)
  { rc = wl_print(cfg, "(uint64 %llu)", value->counter); }
  else if (type == DS_TYPE_DERIVE)
  { rc = wl_print(cfg, "(int64 %"PRIi64")", value->derive); }
  else if (type == DS_TYPE_ABSOLUTE)
  { rc = wl_print(cfg, "(uint64 %"PRIu64")", value->absolute); }
  else
  { rc = -1; }
  return(rc);
}

static
int wl_flush (cdtime_t timeout, const char *identifier, user_data_t *data)
{
  int rc = 0;
  double t0, t1;
  wl_data_t *cfg;

  WL_UNUSED(identifier);
  INFO("write_leela plugin: flush(%f) = %d;", (double) timeout, rc);

  if (data == NULL)
  { return(-1); }

  cfg = data->data;
  if (pthread_mutex_lock(&cfg->mutex) != 0)
  { return(-1); }

  t0 = CDTIME_T_TO_DOUBLE(cfg->ctime);
  t1 = CDTIME_T_TO_DOUBLE(cdtime());
  if (timeout == 0 || t0 + timeout > t1)
  { rc = wl_send(cfg); }

  pthread_mutex_unlock(&cfg->mutex);
  return(rc);
}

static
int wl_print_metric(const data_set_t *ds, const value_list_t *vl, wl_data_t *cfg, const gauge_t *rates, int index)
{
  int rc = (cfg->sndbufoff == 0 ? wl_print(cfg, "using (%s) attr put %s \"", cfg->tree, cfg->guid)
                                : wl_print(cfg, "attr put %s \"", cfg->guid))
         | wl_print_name(cfg, vl, DS_TYPE_TO_STRING(ds->ds[index].type))
         | wl_print(cfg, "\" [")
         | wl_print_time(cfg, vl->time)
         | wl_print(cfg, "] ")
         | wl_print_value(cfg, ds->ds[index].type, &vl->values[index])
         | wl_print(cfg, ", ");

  if (rc == 0 && ds->ds[index].type != DS_TYPE_GAUGE)
  {
    rc = wl_print(cfg, "attr put %s \"", cfg->guid)
         | wl_print_name(cfg, vl, "rate")
         | wl_print(cfg, "\" [")
         | wl_print_time(cfg, vl->time)
         | wl_print(cfg, "] ")
         | wl_print(cfg, "(double %g)", (double) rates[index])
         | wl_print(cfg, ", ");
  }

  return(rc);
}

static
int wl_write (const data_set_t *ds, const value_list_t *vl, user_data_t *data)
{
  int k, rc;
  size_t state;
  wl_data_t *cfg;
  gauge_t *rates;

  if (data == NULL)
  { return(-1); }

  cfg = (wl_data_t *) data->data;

  rates = uc_get_rate(ds, vl);
  if (rates == NULL)
  {
    ERROR("write_leela plugin: uc_get_rate failure");
    return(-1);
  }
  if (pthread_mutex_lock(&cfg->mutex) != 0)
  {
    sfree(rates);
    ERROR("write_leela plugin: error acquiring exclusive lock");
    return(-1);
  }

  for (k=0; k<vl->values_len; k+=1)
  {
    state = cfg->sndbufoff;
    rc    = wl_print_metric(ds, vl, cfg, rates, k);
    if (rc > 0)
    {
      k             -= 1;
      cfg->sndbufoff = state;
      rc             = wl_send(cfg);
    }

    if (rc != 0)
    { break; }
  }

  pthread_mutex_unlock(&cfg->mutex);
  sfree(rates);
  return(rc != 0 ? -1 : 0);
}

static
int wl_check_guid(wl_data_t *cfg)
{
  int rc               = -1;
  lql_cursor_t *cursor = NULL;

  cursor = leela_lql_cursor_init(cfg->ctx, cfg->user, cfg->pass, cfg->timeout);
  if (cursor != NULL && wl_print(cfg, "using (%s) name %s;", cfg->tree, cfg->guid) == 0)
  {
    if (leela_lql_cursor_execute(cursor, cfg->sndbuf) == LEELA_OK
        && leela_lql_cursor_next(cursor) == LEELA_OK
        && leela_lql_fetch_type(cursor) == LQL_NAME_MSG
        && leela_lql_cursor_next(cursor) == LEELA_EOF)
    { rc = 0; }
  }

  if (cursor != NULL)
  { leela_lql_cursor_close(cursor); }
  wl_reset_buffer(cfg);

  return(rc);
}

static
leela_endpoint_t **wl_parse_cluster (oconfig_item_t *item)
{
  int k;
  leela_endpoint_t **cluster = (leela_endpoint_t **) malloc(sizeof(leela_endpoint_t *) * (item->values_num + 1));
  if (cluster != NULL)
  {
    for (k=0; k<item->values_num; k+=1)
    {
      cluster[k] = leela_endpoint_load(item->values[k].value.string);
      if (cluster[k] == NULL)
      {
        ERROR("write_leela plugin: error parsing cluster: %s", item->values[k].value.string);
        wl_cluster_free(cluster);
        return(NULL);
      }
    }
    cluster[k] = NULL;
  }
  return(cluster);
}

static
int wl_cfg (oconfig_item_t *cfg)
{
  int k;
  user_data_t data;
  wl_data_t *leela_cfg;
  leela_endpoint_t **cluster = NULL;

  leela_cfg = (wl_data_t *) malloc(sizeof(wl_data_t));
  if (leela_cfg == NULL)
  {
    ERROR("write_leela plugin: malloc error");
    return(-1);
  }
  leela_cfg->ctx        = NULL;
  leela_cfg->user       = NULL;
  leela_cfg->pass       = NULL;
  leela_cfg->tree       = NULL;
  leela_cfg->guid       = NULL;
  leela_cfg->timeout    = 60000;
  leela_cfg->sndbuf     = NULL;
  leela_cfg->ctime      = cdtime();
  leela_cfg->sndbufoff  = 0;
  leela_cfg->sndbuflen  = 64 * 1024;
  if (pthread_mutex_init(&leela_cfg->mutex, NULL) != 0)
  {
    sfree(leela_cfg);
    ERROR("write_leela plugin: malloc error");
    return(-1);
  }

  for (k=0; k<cfg->children_num; k+=1)
  {
    oconfig_item_t *item = cfg->children + k;
    if (strcasecmp("cluster", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    {
      if (cluster != NULL)
      {
        ERROR("write_leela plugin: multiple cluster configuration found");
        wl_cluster_free(cluster);
        wl_data_free(leela_cfg);
        return(-1);
      }
      cluster = wl_parse_cluster(item);
      if (cluster == NULL)
      {
        wl_data_free(leela_cfg);
        return(-1);
      }
    }
    else if (strcasecmp("user", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    { leela_cfg->user = wl_strdup(item->values[0].value.string); }
    else if (strcasecmp("tree", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    { leela_cfg->tree = wl_strdup(item->values[0].value.string); }
    else if (strcasecmp("secret", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    { leela_cfg->pass  = wl_strdup(item->values[0].value.string); }
    else if (strcasecmp("timeout_in_ms", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_NUMBER)
    { leela_cfg->timeout = (int) item->values[0].value.number; }
    else if (strcasecmp("guid", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    { leela_cfg->guid = wl_strdup(item->values[0].value.string); }
    else
    {
      ERROR("write_leela plugin: invalid config: %s", item->key);
      wl_cluster_free(cluster);
      wl_data_free(leela_cfg);
      return(-1);
    }
  }

  leela_cfg->ctx    = leela_lql_context_init((const leela_endpoint_t * const *) cluster);
  leela_cfg->sndbuf = (char *) malloc(sizeof(char) * leela_cfg->sndbuflen);

  if (leela_cfg->ctx == NULL
      || leela_cfg->sndbuf == NULL
      || leela_cfg->user == NULL
      || leela_cfg->pass == NULL
      || leela_cfg->tree == NULL
      || leela_cfg->guid == NULL)
  {
    if (leela_cfg->ctx == NULL)
    { ERROR("write_leela plugin: error initializing leela context"); }
    else if (leela_cfg->sndbuf == NULL)
    { ERROR("write_leela plugin: malloc error"); }
    else
    { ERROR("write_leela plugin: missing required option"); }
    wl_cluster_free(cluster);
    wl_data_free(leela_cfg);
    return(-1);
  }
  wl_reset_buffer(leela_cfg);
  wl_cluster_free(cluster);

  if (wl_check_guid(leela_cfg) != 0)
  {
    ERROR("write_leela plugin: error initializing, unknow guid: %s", leela_cfg->guid);
    wl_data_free(leela_cfg);
    return(-1);
  }

  data.data      = leela_cfg;
  data.free_func = wl_data_free;
  plugin_register_write("write_leela", wl_write, &data);
  data.free_func = NULL;
  plugin_register_flush("write_leela", wl_flush, &data);

  return(0);
}

void module_register ()
{ plugin_register_complex_config("write_leela", wl_cfg); }

#endif
