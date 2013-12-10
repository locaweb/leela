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

#include <zmq.h>
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
  void            *zmqctx;
  size_t           selector;
  leela_naming_t  *naming;
  pthread_mutex_t  mutex;
};

struct lql_cursor_t {
  void        *socket;
  char        *username;
  char        *secret;
  char        *channel;
  int          timeout;
  uint32_t     elems[2];
  lql_row_type row;
  zmq_msg_t    buffer;
};

static
leela_endpoint_t *__select_endpoint(lql_context_t *ctx)
{
  leela_naming_value_t *snapshot = leela_naming_query(ctx->naming);
  if (snapshot == NULL)
  {
    LEELA_DEBUG0("0 warpdrive instances found!");
    return(NULL);
  }

  if (pthread_mutex_lock(&ctx->mutex) != 0)
  {
    LEELA_DEBUG0("could not acquire an exclusive lock");
    leela_naming_value_free(snapshot);
    return(NULL);
  }

  size_t k;
  leela_naming_value_t *iterator;

  iterator = snapshot;
  for (k=0; iterator!=NULL; k+=1)
  { iterator = iterator->next; }

  iterator      = snapshot;
  ctx->selector = (ctx->selector + 1) % k;
  for (k=ctx->selector; k>0; k-=1)
  { iterator = iterator->next; }
  pthread_mutex_unlock(&ctx->mutex);

  leela_endpoint_t *endpoint = (iterator->endpoint != NULL) ? leela_endpoint_dup(iterator->endpoint) : NULL;
  leela_naming_value_free(snapshot);
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

    LEELA_TRACE("[cursor/%x] > |%s|", cursor, last);
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
  zmq_pollitem_t items[1];
  items[0].socket = cursor->socket;
  items[0].events = ZMQ_POLLIN;
  int rc = zmq_poll(items, 1, cursor->timeout);
  if (rc == 1)
  { return(zmq_msg_recv(&cursor->buffer, cursor->socket, 0)); }
  return(-1);
}

static
int __zmq_recvmsg_str(lql_cursor_t *cursor, char *buff, int buflen)
{
  int rc = __zmq_recvmsg(cursor);
  if (buflen < rc)
  {
    LEELA_DEBUG("[cursor/%x] buffer is too small: %d x %d", cursor, buflen, rc);
    return(-1);
  }

  if (rc != -1)
  {
    memcpy(buff, zmq_msg_data(&cursor->buffer), rc);
    buff[rc] = '\0';
  }

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
bool __zmq_recvmsg_uint32(lql_cursor_t *cursor, uint32_t *out)
{
  char buffer[11];
  buffer[10] = '\0';
  if (__zmq_recvmsg_str(cursor, buffer, 10) != -1)
  {
    *out = (uint32_t) atol(buffer);
    return(true);
  }
  return(false);
}

lql_context_t *leela_lql_context_init(const leela_endpoint_t *zookeeper, const char *path)
{
  pthread_mutex_t mutex;
  if (pthread_mutex_init(&mutex, NULL) != 0)
  { return(NULL); }

  lql_context_t *ctx = (lql_context_t *) malloc(sizeof(lql_context_t));
  if (ctx != NULL)
  {
    ctx->selector = 0;
    ctx->mutex    = mutex;
    ctx->naming   = leela_naming_init(zookeeper, path, LQL_DEFAULT_TIMEOUT);
    ctx->zmqctx   = zmq_ctx_new();
    if (ctx->naming == NULL || ctx->zmqctx == NULL)
    { goto handle_error; }
  }
  return(ctx);

handle_error:
  leela_lql_context_close(ctx);
  return(NULL);
}

lql_cursor_t *leela_lql_cursor_init(lql_context_t *ctx, const char *username, const char *secret, int timeout_in_ms)
{
  leela_endpoint_t *endpoint = NULL;
  char *zmqendpoint          = NULL;
  lql_cursor_t *cursor       = (lql_cursor_t *) malloc(sizeof(lql_cursor_t));
  if (cursor == NULL || zmq_msg_init(&cursor->buffer) == -1)
  {
    free(cursor);
    return(NULL);
  }
  cursor->socket   = NULL;
  cursor->channel  = NULL;
  cursor->username = leela_strdup(username);
  cursor->secret   = leela_strdup(secret);
  cursor->timeout  = (timeout_in_ms == 0 ? LQL_DEFAULT_TIMEOUT : timeout_in_ms);

  if (cursor->username == NULL || cursor->secret == NULL)
  { goto handle_error; }

  endpoint = __select_endpoint(ctx);
  if (endpoint == NULL)
  {
    LEELA_DEBUG0("no warpdrive instance found!");
    goto handle_error;
  }

  zmqendpoint = (endpoint == NULL ? NULL : leela_endpoint_dump(endpoint));
  if (zmqendpoint == NULL)
  { goto handle_error; }
  zmqendpoint[strlen(zmqendpoint) - 1] = '\0';

  cursor->socket = zmq_socket(ctx->zmqctx, ZMQ_REQ);
  if (cursor->socket == NULL)
  { goto handle_error; }

  if (zmq_connect(cursor->socket, zmqendpoint) != 0)
  { goto handle_error; }

  leela_endpoint_free(endpoint);
  free(zmqendpoint);
  return(cursor);

handle_error:
  leela_lql_cursor_close(cursor);
  leela_endpoint_free(endpoint);
  free(zmqendpoint);
  return(NULL);
}

leela_status leela_lql_cursor_execute(lql_cursor_t *cursor, const char *query)
{
  if (cursor == NULL || cursor->channel != NULL)
  { return(LEELA_BADARGS); }

  leela_status rc = LEELA_ERROR;
  if (__zmq_sendmsg_str(cursor, "begin", query, NULL) == -1)
  { goto handle_error; }

  if (! __zmq_recvmsg_done(cursor))
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
  if (cursor == NULL || cursor->channel == NULL)
  { return(LEELA_BADARGS); }

  char buffer[5];
  if (cursor->elems[0] == 0)
  {
    if (__zmq_sendmsg_str(cursor, "fetch", cursor->channel, NULL) == -1)
    { return(LEELA_ERROR); }
  }
  else
  { cursor->elems[0] -= 1; }

  if (__zmq_recvmsg_str(cursor, buffer, 4) == -1)
  { return(LEELA_ERROR); }
  if (strncmp(buffer, "list", 4) == 0)
  {
    if (! __zmq_recvmsg_uint32(cursor, cursor->elems))
    { return(LEELA_ERROR); }
    return(leela_lql_cursor_next(cursor));
  }

  if (strncmp(buffer, "name", 4) == 0)
  { cursor->row = LQL_NAME; }
  else if (strncmp(buffer, "path", 4) == 0)
  {
    cursor->row = LQL_PATH;
    if (! __zmq_recvmsg_uint32(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
  }
  else if (strncmp(buffer, "done", 4) == 0)
  {
    if (zmq_msg_more(&cursor->buffer))
    {
      cursor->elems[0] = 1;
      return(leela_lql_cursor_next(cursor));
    }
    else
    { return(LEELA_EOF); }
  }
  else
  { return(LEELA_ERROR); }
  
  return(LEELA_OK);
}

lql_row_type leela_lql_msg_type(lql_cursor_t *cursor)
{ return(cursor->row); }

lql_name_t *leela_lql_msg_name(lql_cursor_t *cursor)
{
  if (cursor->row != LQL_NAME)
  { return(NULL); }

  lql_name_t *name = (lql_name_t *) malloc(sizeof(lql_name_t));
  if (name != NULL)
  {
    name->user = NULL;
    name->tree = NULL;
    name->name = NULL;

    name->user = __zmq_recvmsg_copystr(cursor);
    name->tree = __zmq_recvmsg_copystr(cursor);
    name->name = __zmq_recvmsg_copystr(cursor);
    if (name->user != NULL
        && name->tree != NULL
        && name->name != NULL)
    { return(name); }
  }

  leela_lql_msg_name_free(name);
  return(NULL);
}

void leela_lql_msg_name_free(lql_name_t *name)
{
  if (name != NULL)
  {
    free(name->user);
    free(name->tree);
    free(name->name);
    free(name);
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
    pthread_mutex_destroy(&ctx->mutex);
    if (ctx->zmqctx != NULL)
    { zmq_ctx_destroy(ctx->zmqctx); }
    if (ctx->naming != NULL)
    { leela_naming_shutdown(ctx->naming); }
    free(ctx);
    return(LEELA_OK);
  }
  return(LEELA_ERROR);
}
