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

#include <zmq.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
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
  void      *socket;
  char      *username;
  char      *secret;
  char      *channel;
  int        timeout;
  size_t     elems[2];
  zmq_msg_t *buffer;
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
  for (k=ctx->selector; k!=0; k-=1)
  { iterator = iterator->next; }
  pthread_mutex_unlock(&ctx->mutex);

  return(iterator->endpoint);
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
  LEELA_TRACE("[%x] > %s|", socket, data);
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
    { last = part; }
    else
    { last = signature; }
    part = va_arg(args, const char *);

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
int __zmq_recvmsg(void *socket, zmq_msg_t *msg, int timeout)
{
  zmq_pollitem_t items[1];
  items[0].socket = socket;
  items[0].events = ZMQ_POLLIN;
  int rc = zmq_poll(items, 1, timeout);
  if (rc == 1)
  { return(zmq_msg_recv(socket, msg, 0)); }
  return(-1);
}

static
int __zmq_recvmsg_str(void *socket, zmq_msg_t *msg, char *buff, int buflen, int timeout)
{
  int rc = __zmq_recvmsg(socket, msg, timeout);

  if (buflen < rc)
  {
    LEELA_DEBUG("[%x] buffer is too small: %d x %d", buflen, rc);
    return(-1);
  }

  if (rc != -1)
  {
    memcpy(buff, zmq_msg_data(msg), rc);
    LEELA_TRACE("[%x] < %s|", socket, rc);
    buff[rc] = '\0';
  }

  return(rc);
}

static
char *__zmq_recvmsg_copystr(void *socket, zmq_msg_t *msg, int timeout)
{
  int rc = __zmq_recvmsg(socket, msg, timeout);
  if (rc == -1)
  { return(NULL); }

  size_t buflen = zmq_msg_size(msg);
  char *buffer  = malloc(buflen + 1);
  if (buffer != NULL)
  {
    memcpy(buffer, zmq_msg_data(msg), buflen);
    buffer[buflen] = '\0';
  }
  return(buffer);
}

static
bool __zmq_recvmsg_done(void *socket, zmq_msg_t *msg, int timeout)
{
  char buffer[5];
  if (__zmq_recvmsg_str(socket, msg, buffer, 5, timeout) != -1
      && strcmp(buffer, "done") == 0)
  { return(true); }
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
    ctx->naming   = leela_naming_init(zookeeper, path);
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
  if (cursor == NULL)
  { return(NULL); }
  cursor->socket   = NULL;
  cursor->channel  = NULL;
  cursor->buffer   = NULL;
  cursor->username = leela_strdup(username);
  cursor->secret   = leela_strdup(secret);
  cursor->timeout  = (timeout_in_ms == 0 ? LQL_DEFAULT_TIMEOUT : timeout_in_ms);

  if (cursor->username == NULL || cursor->secret == NULL)
  { goto handle_error; }

  zmq_msg_t buffer;
  if (zmq_msg_init(&buffer) == -1)
  { goto handle_error; }
  cursor->buffer = &buffer;

  endpoint = __select_endpoint(ctx);
  if (endpoint == NULL)
  {
    LEELA_DEBUG0("no warpdrive instance found!");
    goto handle_error;
  }

  zmqendpoint = (endpoint == NULL ? NULL : leela_endpoint_dump(endpoint));
  if (zmqendpoint == NULL)
  { goto handle_error; }

  cursor->socket = zmq_socket(ctx->zmqctx, ZMQ_REQ);
  if (cursor->socket == NULL)
  { goto handle_error; }

  if (zmq_connect(cursor->socket, zmqendpoint) != 0)
  { goto handle_error; }

  leela_endpoint_free(endpoint);
  free(zmqendpoint);
  LEELA_TRACE("cursor has been created: %x", cursor);
  return(cursor);

handle_error:
  leela_lql_cursor_close(cursor);
  leela_endpoint_free(endpoint);
  free(zmqendpoint);
  return(NULL);
}

leela_status leela_lql_execute(lql_cursor_t *cursor, const char *query)
{
  if (cursor->channel != NULL)
  { return(LEELA_BADARGS); }

  leela_status rc = LEELA_ERROR;
  if (__zmq_sendmsg_str(cursor->socket, "begin", query, NULL) == -1)
  { goto handle_error; }

  if (! __zmq_recvmsg_done(cursor->socket, cursor->buffer, cursor->timeout))
  {
    LEELA_DEBUG("error executing statement: %s", query);
    goto handle_error;
  }

  cursor->channel = __zmq_recvmsg_copystr(cursor->socket, cursor->buffer, cursor->timeout);
  if (cursor->channel == NULL)
  {
    LEELA_DEBUG0("protocol error: bad channel!");
    goto handle_error;
  }

  rc = LEELA_OK;

handle_error:
  return(rc);
}

leela_status leela_next(lql_cursor_t *cursor)
{
  if (__zmq_sendmsg_str(cursor, "fetch", cursor->channel) == -1)
  { return(LEELA_ERROR); }

  if (__zmq_recvmsg(cursor->socket, cursor->buffer, cursor->timeout) == -1)
  { return(LEELA_ERROR); }

  return(LEELA_OK);
}

leela_status leela_lql_cursor_close(lql_cursor_t *cursor)
{
  leela_status rc = LEELA_ERROR;
  if (cursor != NULL)
  {
    if (cursor->socket != NULL && cursor->channel != NULL)
    {
      if (__zmq_sendmsg_str(cursor->socket, "close", cursor->channel) != -1)
      {
        if (__zmq_recvmsg_done(cursor->socket, cursor->buffer, cursor->timeout))
        { rc = LEELA_OK; }
      }
    }
    free(cursor->channel);
    free(cursor->username);
    free(cursor->secret);
    if (cursor->buffer != NULL)
    { zmq_msg_close(cursor->buffer); }
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
    { leela_naming_shutdown(ctx->naming, NULL); }
    free(ctx);
    return(LEELA_OK);
  }
  return(LEELA_ERROR);
}
