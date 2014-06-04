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

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <pthread.h>
#include <strings.h>
#include <inttypes.h>
#include <sys/types.h>

#include <collectd/config.h>
#include <collectd/collectd.h>
#include <collectd/common.h>
#include <collectd/utils_cache.h>
#include <collectd/plugin.h>

#include <leela/lql.h>
#include <leela/string.h>
#include <leela/endpoint.h>

#define WL_UNUSED(x) (void) x
#ifndef WL_BUFFSZ
# define WL_BUFFSZ 32768
#endif

typedef struct
{
  char   *prefix;
  char   *sndbuf;
  size_t  sndbufoff;
  size_t  sndbuflen;
} wl_buffer_t;

typedef struct
{
  lql_context_t     *ctx;
  char              *user;
  char              *pass;
  char              *tree;
  char              *prefix;
  int                timeout;
  cdtime_t           ctime;
  pthread_key_t      key;
  leela_endpoint_t **cluster;
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
wl_data_t *wl_leela_cfg;

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
int wl_send (wl_data_t *cfg, wl_buffer_t *buff)
{
  int rc          = -1;
  size_t offset   = buff->sndbufoff;
  int t0          = time(0);

  if (offset < 2)
  { return(0); }

  lql_cursor_t *cursor = leela_lql_cursor_init_default(cfg->ctx);
  if (cursor == NULL)
  {
    ERROR("write_leela plugin: error initializing cursor");
    return(-1);
  }

  buff->sndbuf[offset-2] = ';';
  buff->sndbuf[offset-1] = '\0';
  if (leela_lql_cursor_execute(cursor, buff->sndbuf) == LEELA_OK
      && leela_lql_cursor_next(cursor) == LEELA_EOF)
  { rc = 0; }
  leela_lql_cursor_close(cursor);

  if (rc != 0)
  { ERROR("write_leela plugin: wl_send() != 0 [size:%zu, timing:%ds]", offset, (int) (time(0) - t0)); }
  else
  {
    buff->sndbufoff = 0;
    INFO("write_leela plugin: wl_send() = 0 [size:%zu, timing:%ds]", offset, (int) (time(0) - t0));
  }
  return(rc);
}

static
void wl_data_free (void *ptr)
{
  wl_data_t *data = (wl_data_t *) ptr;
  if (ptr != NULL)
  {
    wl_cluster_free(data->cluster);
    leela_lql_context_close(data->ctx);
    sfree(data->prefix);
    sfree(data->user);
    sfree(data->pass);
    sfree(data->tree);
    sfree(data);
  }
}

static
void wl_buffer_free(void *ptr)
{
  if (ptr != NULL)
  { free(((wl_buffer_t *)ptr)->sndbuf); }
  free(ptr);
}

static
wl_buffer_t *wl_buffer_alloc (wl_data_t *cfg)
{
  wl_buffer_t *buff = (wl_buffer_t *) malloc(sizeof(wl_buffer_t));
  if (buff != NULL)
  {
    buff->sndbuflen = WL_BUFFSZ;
    buff->sndbufoff = 0;
    buff->prefix    = cfg->prefix;
    buff->sndbuf    = (char *) malloc(buff->sndbuflen);
    if (buff->sndbuf == NULL)
    {
      wl_buffer_free(buff);
      buff = NULL;
    }
  }

  if (buff == NULL)
  { ERROR("write_leela plugin: malloc error"); }

  return(buff);
}

static
wl_buffer_t *wl_get_buffer(wl_data_t *cfg)
{
  wl_buffer_t *buff = (wl_buffer_t *) pthread_getspecific(cfg->key);
  if (buff == NULL)
  {
    buff = wl_buffer_alloc(cfg);
    if (buff != NULL)
    { pthread_setspecific(cfg->key, (void *) buff); }
  }
  return(buff);
}

static
int wl_print (wl_buffer_t *buff, const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  size_t avail  = (buff->sndbuflen > buff->sndbufoff) ? (buff->sndbuflen - buff->sndbufoff) : 0;
  size_t offset = (buff->sndbuflen > buff->sndbufoff) ? buff->sndbufoff : 0;
  int rc        = vsnprintf(buff->sndbuf + offset, avail, fmt, ap);
  if (rc > 0 && ((size_t) rc) < avail)
  {
    buff->sndbufoff += rc;
    rc = 0;
  }
  va_end(ap);

  return(rc);
}

static
int wl_print_name (wl_buffer_t *buff, const value_list_t *vl, const data_source_t *ds)
{
  char v_plugin[DATA_MAX_NAME_LEN];
  char v_plugin_instance[DATA_MAX_NAME_LEN];
  char v_type[DATA_MAX_NAME_LEN];
  char v_type_instance[DATA_MAX_NAME_LEN];
  char v_dataname[DATA_MAX_NAME_LEN];

  return(wl_print(buff,
                  "%s%s%s%s/%s%s%s/%s",
                  (buff->prefix == NULL ? "" : buff->prefix),
                  wl_unquote(v_plugin, DATA_MAX_NAME_LEN, vl->plugin),
                  (wl_blank(vl->plugin_instance) ? "" : "-"),
                  (wl_blank(vl->plugin_instance) ? "" : wl_unquote(v_plugin_instance, DATA_MAX_NAME_LEN, vl->plugin_instance)),
                  wl_unquote(v_type, DATA_MAX_NAME_LEN, vl->type),
                  (wl_blank(vl->type_instance) ? "" : "-"),
                  (wl_blank(vl->type_instance) ? "" : wl_unquote(v_type_instance, DATA_MAX_NAME_LEN, vl->type_instance)),
                  (ds->name == NULL ? wl_unquote(v_dataname, DATA_MAX_NAME_LEN, DS_TYPE_TO_STRING(ds->type)) : wl_unquote(v_dataname, DATA_MAX_NAME_LEN, ds->name))));
}

static
int wl_print_time (wl_buffer_t *buff, const cdtime_t cdtime)
{
  struct timespec time;
  CDTIME_T_TO_TIMESPEC(cdtime, &time);
  return(wl_print(buff, "%ld.%ld", (long) time.tv_sec, time.tv_nsec));
}

static
int wl_print_metric(const data_set_t *ds, const value_list_t *vl, wl_data_t *cfg, wl_buffer_t *buff, const gauge_t *rates, int index)
{
  if (leela_check_guid(vl->host) != 0)
  {
    ERROR("invalid hostname [= guid]; ignoring metrics from plugin/type: %s/%s", vl->plugin, vl->type);
    return(0);
  }

  int rc = (buff->sndbufoff == 0 ? wl_print(buff, "using (%s) attr put %s \"", cfg->tree, vl->host)
                                 : wl_print(buff, "attr put %s \"", vl->host));
  
  if (rc == 0 && ds->ds[index].type == DS_TYPE_GAUGE)
  {
    rc = wl_print_name(buff, vl, &ds->ds[index])
       | wl_print(buff, "\" [")
       | wl_print_time(buff, vl->time)
       | wl_print(buff, "] ")
       | wl_print(buff, "(double %g)", vl->values[index].gauge)
       | wl_print(buff, ", ");
  }
  else if (rc == 0)
  {
    rc = wl_print_name(buff, vl, &ds->ds[index])
       | wl_print(buff, "\" [")
       | wl_print_time(buff, vl->time)
       | wl_print(buff, "] ")
       | wl_print(buff, "(double %g)", (double) rates[index])
       | wl_print(buff, ", ");
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
  wl_buffer_t *buff;

  if (data == NULL)
  { return(-1); }

  cfg   = (wl_data_t *) data->data;
  buff  = wl_get_buffer(cfg);
  rates = uc_get_rate(ds, vl);

  if (buff == NULL)
  { return(-1); }

  if (rates == NULL)
  {
    ERROR("write_leela plugin: uc_get_rate failure");
    return(-1);
  }

  for (k=0; k<ds->ds_num; k+=1)
  {
    state = buff->sndbufoff;
    rc    = wl_print_metric(ds, vl, cfg, buff, rates, k);
    if (rc > 0)
    {
      k              -= 1;
      buff->sndbufoff = state;
      rc              = wl_send(cfg, buff);
    }

    if (rc != 0)
    { break; }
  }

  sfree(rates);
  return(rc != 0 ? -1 : 0);
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
int wl_init ()
{
  user_data_t data;

  if (wl_leela_cfg == NULL)
  { return(-1); }

  wl_leela_cfg->ctx = leela_lql_context_init((const leela_endpoint_t * const *) wl_leela_cfg->cluster, wl_leela_cfg->user, wl_leela_cfg->pass, wl_leela_cfg->timeout);
  if (wl_leela_cfg->ctx == NULL)
  {
    ERROR("write_leela plugin: error initializing leela context");
    wl_data_free(wl_leela_cfg);
    return(-1);
  }

  data.data      = wl_leela_cfg;
  data.free_func = wl_data_free;
  plugin_register_write("write_leela", wl_write, &data);
  return(0);
}

static
int wl_cfg (oconfig_item_t *cfg)
{
  int k;
  wl_data_t *leela_cfg;

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
  leela_cfg->prefix     = NULL;
  leela_cfg->timeout    = 60000;
  leela_cfg->ctime      = cdtime();
  leela_cfg->cluster    = NULL;

  if (pthread_key_create(&leela_cfg->key, wl_buffer_free) != 0)
  {
    ERROR("write_leela plugin: error initializing thread local storage");
    return(-1);
  }

  for (k=0; k<cfg->children_num; k+=1)
  {
    oconfig_item_t *item = cfg->children + k;
    if (strcasecmp("cluster", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    {
      if (leela_cfg->cluster != NULL)
      {
        ERROR("write_leela plugin: multiple cluster configuration found");
        wl_data_free(leela_cfg);
        return(-1);
      }
      leela_cfg->cluster = wl_parse_cluster(item);
      if (leela_cfg->cluster == NULL)
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
    else if (strcasecmp("prefix", item->key) == 0 && item->values[0].type == OCONFIG_TYPE_STRING)
    { leela_cfg->prefix = wl_strdup(item->values[0].value.string); }
    else if (strcasecmp("guid", item->key) != 0) // ignoring obsolete guid parameter
    {
      ERROR("write_leela plugin: invalid config: %s", item->key);
      wl_data_free(leela_cfg);
      return(-1);
    }
  }

  if (leela_cfg->user == NULL
      || leela_cfg->pass == NULL
      || leela_cfg->tree == NULL)
  {
    ERROR("write_leela plugin: missing required option (user|pass|tree)");
    wl_data_free(leela_cfg);
    return(-1);
  }

  wl_leela_cfg = leela_cfg;
  plugin_register_init("write_leela", wl_init);

  return(0);
}

void module_register ()
{
  wl_leela_cfg = NULL;
  plugin_register_complex_config("write_leela", wl_cfg);
}

#endif
