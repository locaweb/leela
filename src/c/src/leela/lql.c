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

#include <zmq.h>
#include <time.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <inttypes.h>
#include "leela/lql.h"
#include "leela/debug.h"
#include "leela/status.h"
#include "leela/string.h"
#include "leela/naming.h"

#define DEBUG 1

struct lql_context_t
{
  void             *zmqctx;
  size_t            offset;
  leela_naming_t   *naming;
  leela_endpoint_t *endpoint;
  pthread_mutex_t   mutex;
};

struct lql_cursor_t {
  void        *socket;
  char        *username;
  char        *secret;
  char        *channel;
  int          feedback;
  int          timeout;
  uint32_t     elems[2];
  lql_row_type row;
  zmq_msg_t    buffer;
};

static
leela_endpoint_t *__select_endpoint(lql_context_t *ctx)
{
  leela_naming_cluster_t *snapshot = leela_naming_discover(ctx->naming);
  if (snapshot == NULL)
  {
    LEELA_DEBUG0("0 warpdrive instances found!");
    return(NULL);
  }

  if (pthread_mutex_lock(&ctx->mutex) != 0)
  {
    LEELA_DEBUG0("could not acquire an exclusive lock");
    leela_naming_cluster_free(snapshot);
    return(NULL);
  }

  leela_endpoint_t *endpoint = leela_endpoint_dup(snapshot->endpoint[ctx->offset % snapshot->size]);
  ctx->offset                = (ctx->offset + 1) % snapshot->size;
  pthread_mutex_unlock(&ctx->mutex);
  leela_naming_cluster_free(snapshot);
  return(endpoint);
}

static
char *__signature(lql_cursor_t *cursor, va_list args)
{
  (void) args;
  size_t bufflen  = strlen(cursor->username) + 8;
  char *signature = (char *) malloc(bufflen);
  snprintf(signature, bufflen, "%s:0:0 0", cursor->username);
  return(signature);
}

static
int __zmq_sendmsg_str(lql_cursor_t *cursor, const char *data, ...)
{
  int rc = -1;
  va_list args;

  va_start(args, data);
  char *signature = __signature(cursor, args);
  if (signature == NULL)
  { goto handle_error; }
  va_end(args);

  va_start(args, data);
  const char *part = NULL;
  const char *last = NULL;
  do
  {
    if (part != NULL)
    {
      last = part;
      part = va_arg(args, const char *);
    }
    else
    {
      last = signature;
      part = data;
    }

    LEELA_TRACE("[cursor/%s] > |%s|", cursor->channel == NULL ? "" : cursor->channel, last);
    rc = zmq_send(cursor->socket, last, strlen(last), (part == NULL ? 0 : ZMQ_SNDMORE));
    if (rc == -1)
    { goto handle_error; }
  } while (part != NULL);

handle_error:
  va_end(args);
  free(signature);
  return(rc);
}

static
int __zmq_recvmsg(lql_cursor_t *cursor)
{
  if (cursor->feedback >= 0)
  {
    int feedback = cursor->feedback;
    cursor->feedback = -1;
    return (feedback);
  }

  zmq_pollitem_t items[1];
  items[0].socket = cursor->socket;
  items[0].events = ZMQ_POLLIN;
  int rc = zmq_poll(items, 1, cursor->timeout);
  if (rc == 1)
  {
    if (cursor->elems[1] > 0)
    { cursor->elems[1] -= 1; }
    return(zmq_msg_recv(&cursor->buffer, cursor->socket, 0));
  }
  return(-1);
}

static
int __zmq_recvmsg_str(lql_cursor_t *cursor, char *buff, int buflen)
{
  int rc = __zmq_recvmsg(cursor);
  if (buflen < rc)
  {
    LEELA_DEBUG("[cursor/%s] buffer is too small: %d x %d", cursor->channel, buflen, rc);
    return(-1);
  }

  if (rc != -1)
  {
    memcpy(buff, zmq_msg_data(&cursor->buffer), rc);
    buff[rc] = '\0';
  }

  if (rc != -1)
  { LEELA_TRACE("[cursor/%s] < |%s|", cursor->channel == NULL ? "" : cursor->channel, buff); }
  else
  { LEELA_TRACE("[cursor/%s] < |ERROR:%d|", cursor->channel == NULL ? "" : cursor->channel, rc); }
  return(rc);
}

static
char *__zmq_recvmsg_copystr(lql_cursor_t *cursor)
{
  int rc = __zmq_recvmsg(cursor);
  if (rc == -1)
  { return(NULL); }

  size_t buflen = zmq_msg_size(&cursor->buffer);
  char *buffer  = malloc(buflen + 1);
  if (buffer != NULL)
  {
    memcpy(buffer, zmq_msg_data(&cursor->buffer), buflen);
    buffer[buflen] = '\0';
  }

  LEELA_TRACE("[cursor/%s] < |%s|", cursor->channel == NULL ? "" : cursor->channel, buffer);
  return(buffer);
}

static
bool __zmq_recvmsg_done(lql_cursor_t *cursor)
{
  char buffer[5];
  if (__zmq_recvmsg_str(cursor, buffer, 5) != -1
      && strcmp(buffer, "done") == 0)
  { return(true); }
  return(false);
}

static
bool __zmq_recvmsg_longlong(lql_cursor_t *cursor, long long *out)
{
  char buffer[21];
  buffer[20] = '\0';
  if (__zmq_recvmsg_str(cursor, buffer, 20) != -1)
  {
    *out = (long long) atoll(buffer);
    return(true);
  }
  return(false);
}

static
bool __zmq_recvmsg_uint32(lql_cursor_t *cursor, uint32_t *out)
{
  long long tmp;
  if (__zmq_recvmsg_longlong(cursor, &tmp))
  {
    *out = (uint32_t) tmp;
    return(true);
  }
  return(false);
}

static
bool __zmq_recvmsg_uint64(lql_cursor_t *cursor, uint64_t *out)
{
  long long tmp;
  if (__zmq_recvmsg_longlong(cursor, &tmp))
  {
    *out = (uint64_t) tmp;
    return(true);
  }
  return(false);
}

static
bool __zmq_recvmsg_int32(lql_cursor_t *cursor, int32_t *out)
{
  long long tmp;
  if (__zmq_recvmsg_longlong(cursor, &tmp))
  {
    *out = (int32_t) tmp;
    return(true);
  }
  return(false);
}

static
bool __zmq_recvmsg_int64(lql_cursor_t *cursor, int64_t *out)
{
  long long tmp;
  if (__zmq_recvmsg_longlong(cursor, &tmp))
  {
    *out = (int64_t) tmp;
    return(true);
  }
  return(false);
}

static
bool __zmq_recvmsg_double(lql_cursor_t *cursor, double *out)
{
  char *tmp = __zmq_recvmsg_copystr(cursor);
  if (tmp != NULL)
  {
    *out = strtod(tmp, NULL);
    free(tmp);
    return(true);
  }
  return(false);
}

static
bool __zmq_recvmsg_bool(lql_cursor_t *cursor, bool *out)
{
  char buffer[5];
  if (__zmq_recvmsg_str(cursor, buffer, 5) != -1)
  {
    if (strncmp(buffer, "true", 4) == 0)
    {
      *out = true;
      return(true);
    }
    else if (strncmp(buffer, "false", 5) == 0)
    {
      *out = false;
      return(true);
    }
  }
  return(false);
}

lql_context_t *leela_lql_context_init(const leela_endpoint_t *const *warpdrive)
{
  lql_context_t *ctx = (lql_context_t *) malloc(sizeof(lql_context_t));
  if (ctx != NULL)
  {
    ctx->offset  = 0;
    ctx->zmqctx  = zmq_ctx_new();
    ctx->naming  = leela_naming_init(warpdrive, LQL_DEFAULT_TIMEOUT);
    if (ctx->naming == NULL || ctx->zmqctx == NULL)
    { goto handle_error; }

    if (pthread_mutex_init(&ctx->mutex, NULL) != 0)
    { goto handle_error; }

    bool ok = leela_naming_start(ctx->naming, ctx);
    if (! ok)
    { goto handle_error; }
  }
  return(ctx);

handle_error:
  leela_lql_context_close(ctx);
  return(NULL);
}

lql_cursor_t *leela_lql_cursor_init2(lql_context_t *ctx, const leela_endpoint_t *endpoint, const char *username, const char *secret, int timeout_in_ms)
{
  char *zmqendpoint     = NULL;
  lql_cursor_t *cursor  = (lql_cursor_t *) malloc(sizeof(lql_cursor_t));
  if (cursor == NULL || zmq_msg_init(&cursor->buffer) == -1)
  {
    free(cursor);
    return(NULL);
  }
  cursor->socket   = NULL;
  cursor->channel  = NULL;
  cursor->username = leela_strdup(username);
  cursor->secret   = leela_strdup(secret);
  cursor->elems[0] = 0;
  cursor->elems[1] = 0;
  cursor->feedback = -1;
  cursor->timeout  = (timeout_in_ms == 0 ? LQL_DEFAULT_TIMEOUT : timeout_in_ms);

  if (cursor->username == NULL || cursor->secret == NULL)
  { goto handle_error; }

  zmqendpoint = (endpoint == NULL ? NULL : leela_endpoint_dump(endpoint));
  if (zmqendpoint == NULL)
  { goto handle_error; }
  LEELA_DEBUG("selecting backend: %s", zmqendpoint);

  cursor->socket = zmq_socket(ctx->zmqctx, ZMQ_REQ);
  if (cursor->socket == NULL)
  { goto handle_error; }
  int linger = 0;
  zmq_setsockopt(cursor->socket, ZMQ_LINGER, &linger, sizeof(linger));

  if (zmq_connect(cursor->socket, zmqendpoint) != 0)
  { goto handle_error; }

  free(zmqendpoint);
  return(cursor);

handle_error:
  leela_lql_cursor_close(cursor);
  free(zmqendpoint);
  return(NULL);
}

lql_cursor_t *leela_lql_cursor_init(lql_context_t *ctx, const char *username, const char *secret, int timeout_in_ms)
{
  leela_endpoint_t *endpoint = __select_endpoint(ctx);
  if (endpoint == NULL)
  {
    LEELA_DEBUG0("no warpdrive instances avaialble!");
    return(NULL);
  }

  lql_cursor_t *cursor = leela_lql_cursor_init2(ctx, endpoint, username, secret, timeout_in_ms);
  leela_endpoint_free(endpoint);
  return(cursor);
}

leela_status leela_lql_cursor_execute(lql_cursor_t *cursor, const char *query)
{
  char buffer[4];
  if (cursor == NULL || cursor->channel != NULL)
  { return(LEELA_BADARGS); }

  leela_status rc = LEELA_ERROR;
  if (__zmq_sendmsg_str(cursor, "begin", query, NULL) == -1)
  { goto handle_error; }

  if (__zmq_recvmsg_str(cursor, buffer, 4) == -1)
  { goto handle_error; }

  if (strncmp(buffer, "fail", 4) == 0)
  {
    cursor->feedback = 4;
    cursor->elems[0] = 1;
    cursor->elems[1] = 0;
    return(LEELA_OK);
  }
  else if (!(strncmp(buffer, "done", 4) == 0))
  {
    LEELA_DEBUG("error executing statement: %s", query);
    goto handle_error;
  }

  cursor->channel = __zmq_recvmsg_copystr(cursor);
  if (cursor->channel == NULL)
  {
    LEELA_DEBUG0("protocol error: bad channel!");
    goto handle_error;
  }

  cursor->elems[0] = 0;
  cursor->elems[1] = 0;
  rc = LEELA_OK;

handle_error:
  return(rc);
}

leela_status leela_lql_cursor_next(lql_cursor_t *cursor)
{
  if (cursor == NULL)
  { return(LEELA_BADARGS); }

  if (cursor->channel == NULL && cursor->elems[0] == 0)
  { return(LEELA_EOF); }

  char buffer[6];
  if (cursor->elems[0] == 0 || cursor->elems[1] > 0)
  {
    int frames;
    cursor->elems[1] = 0;
    cursor->elems[0] = 0;
    for (frames=0; zmq_msg_more(&cursor->buffer); frames+=1)
    { __zmq_recvmsg(cursor); }
    if (frames > 0)
    { LEELA_DEBUG("skipping %d frames [invoking next without fetch?]", frames); }
    if (__zmq_sendmsg_str(cursor, "fetch", cursor->channel, NULL) == -1)
    { return(LEELA_ERROR); }
  }
  else
  { cursor->elems[0] -= 1; }

  if (__zmq_recvmsg_str(cursor, buffer, 6) == -1)
  { return(LEELA_ERROR); }
  if (strncmp(buffer, "list", 4) == 0)
  {
    if (! __zmq_recvmsg_uint32(cursor, cursor->elems))
    { return(LEELA_ERROR); }
    return(leela_lql_cursor_next(cursor));
  }

  if (strncmp(buffer, "name", 4) == 0)
  {
    cursor->row      = LQL_NAME_MSG;
    cursor->elems[1] = 4;
  }
  else if (strncmp(buffer, "stat", 4) == 0)
  {
    cursor->row = LQL_STAT_MSG;
    if (! __zmq_recvmsg_uint32(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
  }
  else if (strncmp(buffer, "path", 4) == 0)
  {
    cursor->row = LQL_PATH_MSG;
    if (! __zmq_recvmsg_uint32(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
  }
  else if (strncmp(buffer, "item", 4) == 0)
  {
    cursor->elems[0] = 1;
    return(leela_lql_cursor_next(cursor));
  }
  else if (strncmp(buffer, "n-attr", 6) == 0)
  {
    cursor->row = LQL_NATTR_MSG;
    if (! __zmq_recvmsg_uint32(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
  }
  else if (strncmp(buffer, "k-attr", 6) == 0)
  {
    cursor->row      = LQL_KATTR_MSG;
    cursor->elems[1] = 4;
  }
  else if (strncmp(buffer, "k-attr", 6) == 0)
  {
    cursor->row      = LQL_KATTR_MSG;
    cursor->elems[1] = 3;
  }
  else if (strncmp(buffer, "done", 4) == 0)
  { return(LEELA_EOF); }
  else if (strncmp(buffer, "fail", 4) == 0)
  {
    cursor->row = LQL_FAIL_MSG;
    cursor->elems[1] = 2;
  }
  else
  { return(LEELA_ERROR); }

  return(LEELA_OK);
}

lql_row_type leela_lql_fetch_type(lql_cursor_t *cursor)
{ return(cursor->row); }

lql_name_t *leela_lql_fetch_name(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_NAME_MSG)
  { return(NULL); }

  lql_name_t *name = (lql_name_t *) malloc(sizeof(lql_name_t));
  if (name != NULL)
  {
    name->guid = NULL;
    name->user = NULL;
    name->tree = NULL;
    name->name = NULL;

    name->user = __zmq_recvmsg_copystr(cursor);
    name->tree = __zmq_recvmsg_copystr(cursor);
    name->name = __zmq_recvmsg_copystr(cursor);
    name->guid = __zmq_recvmsg_copystr(cursor);
    if (name->user != NULL
        && name->tree != NULL
        && name->name != NULL)
    { return(name); }
  }

  leela_lql_name_free(name);
  return(NULL);
}

lql_nattr_t *leela_lql_fetch_nattr(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_NATTR_MSG)
  { return(NULL); }

  lql_nattr_t *nattr = (lql_nattr_t *) malloc(sizeof(lql_nattr_t));
  if (nattr != NULL)
  {
    nattr->size  = cursor->elems[1];
    nattr->guid  = __zmq_recvmsg_copystr(cursor);
    nattr->names = (char **) malloc(nattr->size * sizeof(char *));
    if (nattr->names == NULL || nattr->guid == NULL)
    { goto handle_error; }

    for (int k=0; k<nattr->size; k+=1)
    { nattr->names[k] = NULL; }
    for (int k=0; k<nattr->size; k+=1)
    {
      nattr->names[k] = __zmq_recvmsg_copystr(cursor);
      if (nattr->names[k] == NULL)
      { goto handle_error; }
    }
  }
  return(nattr);

handle_error:
  leela_lql_nattr_free(nattr);
  return(NULL);
}

lql_kattr_t *leela_lql_fetch_kattr(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_KATTR_MSG)
  { return(NULL); }

  lql_kattr_t *kattr = (lql_kattr_t *) malloc(sizeof(lql_kattr_t));
  if (kattr != NULL)
  {
    uint32_t vtype = 0;
    kattr->guid    = __zmq_recvmsg_copystr(cursor);
    kattr->name    = __zmq_recvmsg_copystr(cursor);
    kattr->value   = malloc(sizeof(lql_value_t));
    if (kattr->guid == NULL || kattr->name == NULL || kattr->value == NULL)
    { goto handle_error; }

    kattr->value->data.v_str = NULL;
    if (! __zmq_recvmsg_uint32(cursor, &vtype))
    { goto handle_error; }

    switch (vtype)
    {
    case 0:
      kattr->value->vtype = LQL_BOOL_TYPE;
      if (! __zmq_recvmsg_bool(cursor, &kattr->value->data.v_bool))
      { goto handle_error; }
      break;
    case 1:
      kattr->value->vtype      = LQL_TEXT_TYPE;
      kattr->value->data.v_str = __zmq_recvmsg_copystr(cursor);
      if (kattr->value->data.v_str == NULL)
      { goto handle_error; }
      break;
    case 2:
      kattr->value->vtype = LQL_INT32_TYPE;
      if (! __zmq_recvmsg_int32(cursor, &kattr->value->data.v_i32))
      { goto handle_error; }
      break;
    case 3:
      kattr->value->vtype = LQL_INT64_TYPE;
      if (! __zmq_recvmsg_int64(cursor, &kattr->value->data.v_i64))
      { goto handle_error; }
      break;
    case 4:
      kattr->value->vtype = LQL_UINT32_TYPE;
      if (! __zmq_recvmsg_uint32(cursor, &kattr->value->data.v_u32))
      { goto handle_error; }
      break;
    case 5:
      kattr->value->vtype = LQL_UINT64_TYPE;
      if (! __zmq_recvmsg_uint64(cursor, &kattr->value->data.v_u64))
      { goto handle_error; }
      break;
    case 6:
      kattr->value->vtype = LQL_DOUBLE_TYPE;
      if (! __zmq_recvmsg_double(cursor, &kattr->value->data.v_double))
      { goto handle_error; }
      break;
    default:
      goto handle_error;
    };
  }

  return(kattr);

handle_error:
  leela_lql_kattr_free(kattr);
  return(NULL);
}

lql_stat_t *leela_lql_fetch_stat(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_STAT_MSG)
  { return(NULL); }

  lql_stat_t *stat = (lql_stat_t *) malloc(sizeof(lql_stat_t));
  if (stat != NULL)
  {
    uint32_t elems = cursor->elems[1] / 2;
    stat->size  = 0;
    stat->attrs = (lql_tuple2_t *) malloc(elems * sizeof(lql_tuple2_t));

    uint32_t k;
    for (k=0; k<elems; k+=1)
    {
      stat->attrs[k].fst = __zmq_recvmsg_copystr(cursor);
      stat->attrs[k].snd = __zmq_recvmsg_copystr(cursor);
      stat->size          += 1;
      if (stat->attrs[k].fst == NULL
          || stat->attrs[k].snd == NULL)
      { break; }
    }
    if (k == elems)
    { return(stat); }
  }

  leela_lql_stat_free(stat);
  return(NULL);
}

lql_path_t *leela_lql_fetch_path(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_PATH_MSG)
  { return(NULL); }

  lql_path_t *path = (lql_path_t *) malloc(sizeof(lql_path_t));
  if (path != NULL)
  {
    uint32_t elems = cursor->elems[1] / 2;
    path->size     = 0;
    path->entries  = (lql_tuple2_t *) malloc(elems * sizeof(lql_tuple2_t));

    uint32_t k;
    for (k=0; k<elems; k+=1)
    {
      path->entries[k].fst = __zmq_recvmsg_copystr(cursor);
      path->entries[k].snd = __zmq_recvmsg_copystr(cursor);
      path->size          += 1;
      if (path->entries[k].fst == NULL
          || path->entries[k].snd == NULL)
      { break; }
    }
    if (k == elems)
    { return(path); }
  }

  leela_lql_path_free(path);
  return(NULL);
}

lql_fail_t *leela_lql_fetch_fail(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_FAIL_MSG)
  { return(NULL); }

  lql_fail_t *fail = (lql_fail_t *) malloc(sizeof(lql_fail_t));
  if (fail != NULL)
  {
    __zmq_recvmsg_uint32(cursor, &(fail->code));
    fail->message = __zmq_recvmsg_copystr(cursor);
    if (fail->message == NULL)
    {
      leela_lql_fail_free(fail);
      fail = NULL;
    }
  }
  return(fail);
}

void leela_lql_name_free(lql_name_t *name)
{
  if (name != NULL)
  {
    free(name->guid);
    free(name->user);
    free(name->tree);
    free(name->name);
    free(name);
  }
}

void leela_lql_stat_free(lql_stat_t *stat)
{
  if (stat == NULL)
  { return; }

  for (int k=0; k<stat->size; k+=1)
  {
    free(stat->attrs[k].fst);
    free(stat->attrs[k].snd);
  }
  free(stat->attrs);
  free(stat);
}

void leela_lql_path_free(lql_path_t *path)
{
  if (path == NULL)
  { return; }

  for (int k=0; k<path->size; k+=1)
  {
    free(path->entries[k].fst);
    free(path->entries[k].snd);
  }
  free(path->entries);
  free(path);
}

void leela_lql_fail_free(lql_fail_t *fail)
{
  if (fail != NULL)
  {
    free(fail->message);
    free(fail);
  }
}

void leela_lql_nattr_free(lql_nattr_t *nattr)
{
  if (nattr != NULL)
  {
    free(nattr->guid);
    if (nattr->names != NULL)
    {
      for (int k=0; k<nattr->size; k+=1)
      { free(nattr->names[k]); }
      free(nattr->names);
    }
    free(nattr);
  }
}

void leela_lql_kattr_free(lql_kattr_t *kattr)
{
  if (kattr != NULL)
  {
    free(kattr->guid);
    free(kattr->name);
    if (kattr->value != NULL)
    {
      if (kattr->value->vtype == LQL_TEXT_TYPE)
      { free(kattr->value->data.v_str); }
      free(kattr->value);
    }
    free(kattr);
  }
}

leela_status leela_lql_cursor_close(lql_cursor_t *cursor)
{
  leela_status rc = LEELA_ERROR;
  if (cursor != NULL)
  {
    if (cursor->socket != NULL && cursor->channel != NULL)
    {
      rc = LEELA_OK;
      if (__zmq_sendmsg_str(cursor, "close", cursor->channel, NULL) != -1)
      { __zmq_recvmsg_done(cursor); }
    }
    free(cursor->channel);
    free(cursor->username);
    free(cursor->secret);
    zmq_msg_close(&cursor->buffer);
    if (cursor->socket != NULL)
    { zmq_close(cursor->socket); }
    free(cursor);
  }
  return(rc);
}

leela_status leela_lql_context_close(lql_context_t *ctx)
{
  if (ctx != NULL)
  {
    if (ctx->naming != NULL)
    { leela_naming_destroy(ctx->naming); }
    if (ctx->zmqctx != NULL)
    { zmq_ctx_destroy(ctx->zmqctx); }
    pthread_mutex_destroy(&ctx->mutex);
    free(ctx);
    return(LEELA_OK);
  }
  return(LEELA_ERROR);
}
