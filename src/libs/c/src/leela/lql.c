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

#include <zmq.h>
#include <time.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <inttypes.h>
#include "lql.h"
#include "status.h"
#include "string.h"
#include "naming.h"
#include "signature.h"

#define DEBUG 1

struct lql_context_t
{
  void              *zmqctx;
  leela_random_t    *random;
  leela_naming_t    *naming;
  leela_endpoint_t  *endpoint;
  pthread_mutex_t    mutex;
  char              *username;
  leela_signature_t *sig;
  int                timeout;
  unsigned char      nonce[LEELA_SIGNATURE_NONCE_SIZE];
  log_function_f     debug_f;
  void              *debug_data;
  log_function_f     trace_f;
  void              *trace_data;
};


struct lql_cursor_t {
  lql_context_t     *ctx;
  void              *socket;
  char              *username;
  leela_signature_t *sig;
  char              *channel;
  int                feedback;
  int                timeout;
  uint32_t           elems[2];
  lql_row_type       row;
  zmq_msg_t          buffer;
};

void lql_debug (lql_context_t *ctx, const char *fmt, ...)
{
  va_list argp;
  if (ctx != NULL && ctx->debug_f != NULL)
  {
    va_start(argp, fmt);
    ctx->debug_f(ctx->debug_data, fmt, argp);
    va_end(argp);
  }
}

void lql_trace (lql_context_t *ctx, const char *fmt, ...)
{
  va_list argp;
  if (ctx != NULL && ctx->trace_f != NULL)
  {
    va_start(argp, fmt);
    ctx->trace_f(ctx->trace_data, fmt, argp);
    va_end(argp);
  }
}

void leela_lql_value_free__ (lql_value_t *value)
{
  if (value != NULL)
  {
    if (value->vtype == LQL_TEXT_TYPE)
    { free(value->data.v_str); }
    free(value);
  }
}

void leela_lql_tuple2_free_members__ (const lql_tuple2_t *pair)
{
  if (pair != NULL)
  {
    if (pair->fst_finalizer != NULL)
    { pair->fst_finalizer(pair->fst); }
    if (pair->snd_finalizer != NULL)
    { pair->snd_finalizer(pair->snd); }
  }
}

static
int next_nounce__ (lql_cursor_t *cursor, unsigned char nonce[LEELA_SIGNATURE_NONCE_SIZE])
{
  if (pthread_mutex_lock(&cursor->ctx->mutex) != 0)
  {
    LEELA_DEBUG0(cursor->ctx, "next_nounce__: error acquiring the lock, aborting");
    return(-1);
  }
  leela_signature_nonce_next(nonce, cursor->ctx->nonce);
  memcpy(cursor->ctx->nonce, nonce, LEELA_SIGNATURE_NONCE_SIZE);
  pthread_mutex_unlock(&cursor->ctx->mutex);
  return(0);
}

static
char *signature__ (lql_cursor_t *cursor, char *msg)
{
  unsigned char nonce[LEELA_SIGNATURE_NONCE_SIZE];
  unsigned char mac[LEELA_SIGNATURE_SIZE];
  char hexnonce[2 * LEELA_SIGNATURE_NONCE_SIZE + 1];
  char hexmac[2 * LEELA_SIGNATURE_SIZE + 1];
  size_t offset;
  size_t msglen   = strlen(msg);
  char *user      = cursor->username == NULL ? cursor->ctx->username : cursor->username;
  char *fmt       = "%s:%d:%s%s%s";
  int now         = (int) time(0);
  size_t bufflen  = snprintf(NULL, 0, fmt, user, now, "", " ", "")
                  + (2 * LEELA_SIGNATURE_NONCE_SIZE)
                  + (2 * LEELA_SIGNATURE_SIZE)
                  + msglen
                  + 1;
  char *signature = NULL;
  if (next_nounce__(cursor, nonce) != 0)
  { return(NULL); }
  signature = (char *) malloc(bufflen+1);
  if (signature != NULL)
  {
    leela_signature_hexencode(hexnonce, nonce, LEELA_SIGNATURE_NONCE_SIZE);
    offset = snprintf(signature, bufflen, fmt, user, now, hexnonce, ":", msg);
    leela_signature_sign(cursor->sig != NULL ? cursor->sig : cursor->ctx->sig, mac, nonce, signature, offset);
    leela_signature_hexencode(hexmac, mac, LEELA_SIGNATURE_SIZE);
    snprintf(signature, bufflen, fmt, user, now, hexnonce, " ", hexmac);
  }
  return(signature);
}

static
int zmq_sendmsg_str__ (lql_cursor_t *cursor, const char *data, ...)
{
  int rc           = -1;
  size_t msglen    = 0;
  size_t offset    = 0;
  char *sig        = NULL;
  char *msg        = NULL;
  const char *part = NULL;
  const char *last = NULL;
  va_list args;

  va_start(args, data);
  while ((part = va_arg(args, const char *)) != NULL)
  { msglen += strlen(part); }
  msglen += strlen(data);
  va_end(args);

  va_start(args, data);
  msg = (char *) malloc(msglen + 1);
  if (msg == NULL)
  { goto handle_error; }

  offset += strlen(data);
  strncpy(msg, data, msglen);
  while ((part = va_arg(args, const char *)) != NULL)
  {
    strncpy(msg + offset, part, msglen - offset);
    offset += strlen(part);
  }
  msg[msglen] = '\0';
  sig = signature__(cursor, msg);
  va_end(args);

  va_start(args, data);
  if (sig == NULL)
  { goto handle_error; }

  do
  {
    if (part != NULL)
    {
      last = part;
      part = va_arg(args, const char *);
    }
    else
    {
      last = sig;
      part = data;
    }

    LEELA_TRACE2(cursor->ctx, "[cursor/%s] > |%s|", cursor->channel == NULL ? "" : cursor->channel, last);
    rc = zmq_send(cursor->socket, last, strlen(last), (part == NULL ? 0 : ZMQ_SNDMORE));
    if (rc == -1)
    { goto handle_error; }
  } while (part != NULL);

handle_error:
  va_end(args);
  free(msg);
  free(sig);
  return(rc);
}

static
int zmq_recvmsg__ (lql_cursor_t *cursor)
{
  int feedback, rc;
  zmq_pollitem_t items[1];
  if (cursor->feedback >= 0)
  {
    feedback = cursor->feedback;
    cursor->feedback = -1;
    return (feedback);
  }

  items[0].socket = cursor->socket;
  items[0].events = ZMQ_POLLIN;
  rc              = zmq_poll(items, 1, cursor->timeout);
  if (rc == 1)
  {
    if (cursor->elems[1] > 0)
    { cursor->elems[1] -= 1; }
    return(zmq_msg_recv(&cursor->buffer, cursor->socket, 0));
  }
  return(-1);
}

static
int zmq_recvmsg_str__ (lql_cursor_t *cursor, char *buff, int buflen)
{
  int rc = zmq_recvmsg__(cursor);
  if (buflen < rc)
  {
    LEELA_DEBUG3(cursor->ctx, "[cursor/%s] buffer is too small: %d x %d", cursor->channel, buflen, rc);
    return(-1);
  }

  if (rc != -1)
  {
    memcpy(buff, zmq_msg_data(&cursor->buffer), rc);
    buff[rc] = '\0';
  }

  if (rc != -1)
  { LEELA_TRACE2(cursor->ctx, "[cursor/%s] < |%s|", cursor->channel == NULL ? "" : cursor->channel, buff); }
  else
  { LEELA_TRACE2(cursor->ctx, "[cursor/%s] < |ERROR:%d|", cursor->channel == NULL ? "" : cursor->channel, rc); }
  return(rc);
}

static
char *zmq_recvmsg_copystr__ (lql_cursor_t *cursor)
{
  char *buffer;
  size_t buflen;
  int rc = zmq_recvmsg__(cursor);
  if (rc == -1)
  { return(NULL); }

  buflen = zmq_msg_size(&cursor->buffer);
  buffer = (char *) malloc(buflen + 1);
  if (buffer != NULL)
  {
    memcpy(buffer, zmq_msg_data(&cursor->buffer), buflen);
    buffer[buflen] = '\0';
  }

  LEELA_TRACE2(cursor->ctx, "[cursor/%s] < |%s|", cursor->channel == NULL ? "" : cursor->channel, buffer);
  return(buffer);
}

static
bool zmq_recvmsg_done__ (lql_cursor_t *cursor)
{
  char buffer[5];
  if (zmq_recvmsg_str__(cursor, buffer, 5) != -1
      && strcmp(buffer, "done") == 0)
  { return(true); }
  return(false);
}

static
bool zmq_recvmsg_uint64__ (lql_cursor_t *cursor, uint64_t *out)
{
  char buffer[22];
  buffer[21] = '\0';
  if (zmq_recvmsg_str__(cursor, buffer, 21) != -1)
  {
    *out = (uint64_t) atoll(buffer);
    return(true);
  }
  return(false);
}

static
bool zmq_recvmsg_uint32__ (lql_cursor_t *cursor, uint32_t *out)
{
  uint64_t tmp;
  if (zmq_recvmsg_uint64__(cursor, &tmp))
  {
    *out = (uint32_t) tmp;
    return(true);
  }
  return(false);
}

static
bool zmq_recvmsg_int32__ (lql_cursor_t *cursor, int32_t *out)
{
  uint64_t tmp;
  if (zmq_recvmsg_uint64__(cursor, &tmp))
  {
    *out = (int32_t) tmp;
    return(true);
  }
  return(false);
}

static
bool zmq_recvmsg_int64__ (lql_cursor_t *cursor, int64_t *out)
{
  uint64_t tmp;
  if (zmq_recvmsg_uint64__(cursor, &tmp))
  {
    *out = (int64_t) tmp;
    return(true);
  }
  return(false);
}

static
bool zmq_recvmsg_double__ (lql_cursor_t *cursor, double *out)
{
  char *tmp = zmq_recvmsg_copystr__(cursor);
  if (tmp != NULL)
  {
    *out = strtod(tmp, NULL);
    free(tmp);
    return(true);
  }
  return(false);
}

static
double *zmq_recvmsg_copydouble__ (lql_cursor_t *cursor)
{
  double *d = (double *) malloc(sizeof(double));
  if (d != NULL)
  {
    if (zmq_recvmsg_double__(cursor, d))
    { return(d); }
  }
  free(d);
  return(NULL);
}

static
bool zmq_recvmsg_bool__ (lql_cursor_t *cursor, bool *out)
{
  char buffer[5];
  if (zmq_recvmsg_str__(cursor, buffer, 5) != -1)
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

lql_value_t *zmq_recvmsg_copylqlvalue__ (lql_cursor_t *cursor)
{
  int32_t vtype      = 0;
  lql_value_t *value = (lql_value_t *) malloc(sizeof(lql_value_t));
  if (value == NULL)
  { return(NULL); }

  value->data.v_str = NULL;
  if (! zmq_recvmsg_int32__(cursor, &vtype))
  { goto handle_error; }

  switch (vtype)
  {
  case -1:
    value->vtype = LQL_NIL_TYPE;
    if (zmq_recvmsg__(cursor) == -1)
    { goto handle_error; }
    break;
  case 0:
    value->vtype = LQL_BOOL_TYPE;
    if (! zmq_recvmsg_bool__(cursor, &value->data.v_bool))
    { goto handle_error; }
    break;
  case 1:
    value->vtype      = LQL_TEXT_TYPE;
    value->data.v_str = zmq_recvmsg_copystr__(cursor);
    if (value->data.v_str == NULL)
    { goto handle_error; }
    break;
  case 2:
    value->vtype = LQL_INT32_TYPE;
    if (! zmq_recvmsg_int32__(cursor, &value->data.v_i32))
    { goto handle_error; }
    break;
  case 3:
    value->vtype = LQL_INT64_TYPE;
    if (! zmq_recvmsg_int64__(cursor, &value->data.v_i64))
    { goto handle_error; }
    break;
  case 4:
    value->vtype = LQL_UINT32_TYPE;
    if (! zmq_recvmsg_uint32__(cursor, &value->data.v_u32))
    { goto handle_error; }
    break;
  case 5:
    value->vtype = LQL_UINT64_TYPE;
    if (! zmq_recvmsg_uint64__(cursor, &value->data.v_u64))
    { goto handle_error; }
    break;
  case 6:
    value->vtype = LQL_DOUBLE_TYPE;
    if (! zmq_recvmsg_double__(cursor, &value->data.v_double))
    { goto handle_error; }
    break;
  default:
    goto handle_error;
  };

  return(value);

handle_error:
  leela_lql_value_free__(value);
  return(NULL);
}

lql_context_t *leela_lql_context_init (const leela_endpoint_t *const *warpdrive, const char *username, const char *secret, int timeout_in_ms)
{ return(leela_lql_context_init2(warpdrive, username, secret, timeout_in_ms, NULL, NULL, NULL, NULL)); }

lql_context_t *leela_lql_context_init2 (const leela_endpoint_t *const *warpdrive, const char *username, const char *secret, int timeout_in_ms, log_function_f debug_f, void *debug_data, log_function_f trace_f, void *trace_data)
{
  unsigned char seed[LEELA_SIGNATURE_SEED_SIZE];
  lql_context_t *ctx = (lql_context_t *) malloc(sizeof(lql_context_t));
  if (ctx != NULL)
  {
    memset(seed, 0, LEELA_SIGNATURE_SEED_SIZE);
    leela_signature_hexdecode(seed, LEELA_MIN(LEELA_SIGNATURE_SEED_SIZE, strlen(secret) / 2), secret);

    if (pthread_mutex_init(&ctx->mutex, NULL) != 0)
    {
      LEELA_DEBUG0(ctx, "error initializing mutex");
      free(ctx);
      return(NULL);
    }
    ctx->random      = leela_random_init();
    ctx->zmqctx      = zmq_ctx_new();
    ctx->timeout     = timeout_in_ms <= 0 ? LQL_DEFAULT_TIMEOUT : timeout_in_ms;
    ctx->naming      = leela_naming_init(warpdrive, ctx->timeout / 1000);
    ctx->username    = leela_strdup(username);
    ctx->sig         = leela_signature_init(seed);
    ctx->debug_f     = debug_f;
    ctx->debug_data  = debug_data;
    ctx->trace_f     = trace_f;
    ctx->trace_data  = trace_data;

    if (ctx->random == NULL)
    {
      LEELA_DEBUG0(ctx, "error initializing random subsystem");
      goto handle_error;
    }

    if (leela_random_read(ctx->random, ctx->nonce, LEELA_SIGNATURE_NONCE_SIZE) != 0)
    {
      LEELA_DEBUG0(ctx, "error initializing nonce");
      goto handle_error;
    }

    if (ctx->naming == NULL
        || ctx->random == NULL
        || ctx->zmqctx == NULL
        || ctx->username == NULL
        || ctx->sig == NULL)
    {
      LEELA_DEBUG0(ctx, "malloc error");
      goto handle_error;
    }

    bool ok = leela_naming_start(ctx->naming, ctx);
    if (! ok)
    {
      LEELA_DEBUG0(ctx, "error initializing naming thread");
      goto handle_error;
    }
  }
  return(ctx);

handle_error:
  leela_lql_context_close(ctx);
  return(NULL);
}

int leela_lql_cursor_restart (lql_cursor_t *cursor)
{
  if (cursor == NULL)
  { return(-1); }

  free(cursor->channel);
  cursor->channel  = NULL;
  cursor->elems[0] = 0;
  cursor->elems[1] = 0;
  cursor->feedback = -1;
  return(0);
}

lql_cursor_t *leela_lql_cursor_init_on (lql_context_t *ctx, const leela_endpoint_t *endpoint, const char *username, const char *secret, int timeout_in_ms)
{
  int linger            = 0;
  char *zmqendpoint     = NULL;
  lql_cursor_t *cursor  = (lql_cursor_t *) malloc(sizeof(lql_cursor_t));
  unsigned char seed[LEELA_SIGNATURE_SEED_SIZE];

  if (cursor == NULL || zmq_msg_init(&cursor->buffer) == -1)
  {
    free(cursor);
    return(NULL);
  }

  if (secret != NULL)
  {
    memset(seed, 0, LEELA_SIGNATURE_SEED_SIZE);
    leela_signature_hexdecode(seed, LEELA_MIN(LEELA_SIGNATURE_SEED_SIZE, strlen(secret) / 2), secret);
  }

  cursor->ctx      = ctx;
  cursor->socket   = NULL;
  cursor->channel  = NULL;
  cursor->username = username != NULL ? leela_strdup(username) : NULL;
  cursor->sig      = username != NULL ? leela_signature_init(seed) : NULL;
  cursor->elems[0] = 0;
  cursor->elems[1] = 0;
  cursor->feedback = -1;
  cursor->timeout  = (timeout_in_ms == 0 ? ctx->timeout : timeout_in_ms);

  zmqendpoint = (endpoint == NULL ? NULL : leela_endpoint_dump(endpoint));
  if (zmqendpoint == NULL)
  { goto handle_error; }
  LEELA_DEBUG1(ctx, "selecting backend: %s", zmqendpoint);

  cursor->socket = zmq_socket(ctx->zmqctx, ZMQ_REQ);
  if (cursor->socket == NULL)
  { goto handle_error; }
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

lql_cursor_t *leela_lql_cursor_init_default (lql_context_t *ctx)
{ return(leela_lql_cursor_init(ctx, NULL, NULL, 0)); }

lql_cursor_t *leela_lql_cursor_init (lql_context_t *ctx, const char *username, const char *secret, int timeout_in_ms)
{
  lql_cursor_t *cursor;
  leela_endpoint_t *endpoint = leela_naming_select(ctx->naming);
  if (endpoint == NULL)
  {
    LEELA_DEBUG0(ctx, "no warpdrive instances avaialble!");
    return(NULL);
  }

  cursor = leela_lql_cursor_init_on(ctx, endpoint, username, secret, timeout_in_ms);
  leela_endpoint_free(endpoint);
  return(cursor);
}

leela_status leela_lql_cursor_execute (lql_cursor_t *cursor, const char *query)
{
  char buffer[4];
  leela_status rc;
  if (cursor == NULL || cursor->channel != NULL)
  { return(LEELA_BADARGS); }

  rc = LEELA_ERROR;
  if (zmq_sendmsg_str__(cursor, "begin", query, NULL) == -1)
  { goto handle_error; }

  if (zmq_recvmsg_str__(cursor, buffer, 4) == -1)
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
    LEELA_DEBUG1(cursor->ctx, "error executing statement: %s", query);
    goto handle_error;
  }

  cursor->channel = zmq_recvmsg_copystr__(cursor);
  if (cursor->channel == NULL)
  {
    LEELA_DEBUG0(cursor->ctx, "protocol error: bad channel!");
    goto handle_error;
  }

  cursor->elems[0] = 0;
  cursor->elems[1] = 0;
  rc = LEELA_OK;

handle_error:
  return(rc);
}

leela_status leela_lql_cursor_next (lql_cursor_t *cursor)
{
  int frames;
  char buffer[6];
  if (cursor == NULL)
  { return(LEELA_BADARGS); }

  if (cursor->channel == NULL && cursor->elems[0] == 0)
  { return(LEELA_EOF); }

  if (cursor->elems[0] == 0 || cursor->elems[1] > 0)
  {
    cursor->elems[1] = 0;
    cursor->elems[0] = 0;
    for (frames=0; zmq_msg_more(&cursor->buffer); frames+=1)
    { zmq_recvmsg__(cursor); }
    if (frames > 0)
    { LEELA_DEBUG1(cursor->ctx, "skipping %d frames [invoking next without fetch?]", frames); }
    if (zmq_sendmsg_str__(cursor, "fetch", cursor->channel, NULL) == -1)
    { return(LEELA_ERROR); }
  }
  else
  { cursor->elems[0] -= 1; }

  if (zmq_recvmsg_str__(cursor, buffer, 6) == -1)
  { return(LEELA_ERROR); }
  if (strncmp(buffer, "list", 4) == 0)
  {
    if (! zmq_recvmsg_uint32__(cursor, cursor->elems))
    { return(LEELA_ERROR); }
    return(leela_lql_cursor_next(cursor));
  }

  if (strncmp(buffer, "name", 4) == 0)
  {
    cursor->row      = LQL_NAME_MSG;
    cursor->elems[1] = 5;
  }
  else if (strncmp(buffer, "stat", 4) == 0)
  {
    cursor->row = LQL_STAT_MSG;
    if (! zmq_recvmsg_uint32__(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
    cursor->elems[1] = cursor->elems[1] * 2;
  }
  else if (strncmp(buffer, "path", 4) == 0)
  {
    cursor->row = LQL_PATH_MSG;
    if (! zmq_recvmsg_uint32__(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
    cursor->elems[1] = cursor->elems[1] * 2;
  }
  else if (strncmp(buffer, "item", 4) == 0)
  {
    cursor->elems[0] = 1;
    return(leela_lql_cursor_next(cursor));
  }
  else if (strncmp(buffer, "n-attr", 6) == 0)
  {
    cursor->row = LQL_NATTR_MSG;
    if (! zmq_recvmsg_uint32__(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
  }
  else if (strncmp(buffer, "k-attr", 6) == 0)
  {
    cursor->row      = LQL_KATTR_MSG;
    cursor->elems[1] = 4;
  }
  else if (strncmp(buffer, "t-attr", 6) == 0)
  {
    cursor->row      = LQL_TATTR_MSG;
    if (! zmq_recvmsg_uint32__(cursor, cursor->elems + 1))
    { return(LEELA_ERROR); }
    cursor->elems[1] = cursor->elems[1] * 2 + 2;
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

lql_row_type leela_lql_fetch_type (lql_cursor_t *cursor)
{ return(cursor->row); }

lql_name_t *leela_lql_fetch_name (lql_cursor_t *cursor)
{
  lql_name_t *name;
  if (cursor->row != LQL_NAME_MSG)
  { return(NULL); }

  name = (lql_name_t *) malloc(sizeof(lql_name_t));
  if (name != NULL)
  {
    name->user = NULL;
    name->tree = NULL;
    name->kind = NULL;
    name->name = NULL;
    name->guid = NULL;

    name->user = zmq_recvmsg_copystr__(cursor);
    name->tree = zmq_recvmsg_copystr__(cursor);
    name->kind = zmq_recvmsg_copystr__(cursor);
    name->name = zmq_recvmsg_copystr__(cursor);
    name->guid = zmq_recvmsg_copystr__(cursor);
    if (name->user != NULL
        && name->tree != NULL
        && name->name != NULL
        && name->guid != NULL)
    { return(name); }
  }

  leela_lql_name_free(name);
  return(NULL);
}

lql_nattr_t *leela_lql_fetch_nattr (lql_cursor_t *cursor)
{
  int k;
  lql_nattr_t *nattr;
  if (cursor->row != LQL_NATTR_MSG)
  { return(NULL); }

  nattr = (lql_nattr_t *) malloc(sizeof(lql_nattr_t));
  if (nattr != NULL)
  {
    nattr->size  = cursor->elems[1];
    nattr->guid  = zmq_recvmsg_copystr__(cursor);
    nattr->names = (char **) malloc(nattr->size * sizeof(char *));
    if (nattr->names == NULL || nattr->guid == NULL)
    { goto handle_error; }

    for (k=0; k<nattr->size; k+=1)
    { nattr->names[k] = NULL; }
    for (k=0; k<nattr->size; k+=1)
    {
      nattr->names[k] = zmq_recvmsg_copystr__(cursor);
      if (nattr->names[k] == NULL)
      { goto handle_error; }
    }
  }
  return(nattr);

handle_error:
  leela_lql_nattr_free(nattr);
  return(NULL);
}

lql_tattr_t *leela_lql_fetch_tattr (lql_cursor_t *cursor)
{
  uint32_t k;
  uint32_t elems;
  lql_tattr_t *tattr;
  if (cursor->row != LQL_TATTR_MSG)
  { return(NULL); }

  tattr = (lql_tattr_t *) malloc(sizeof(lql_tattr_t));
  if (tattr != NULL)
  {
    elems = (cursor->elems[1] - 2) / 2;
    tattr->guid    = zmq_recvmsg_copystr__(cursor);
    tattr->name    = zmq_recvmsg_copystr__(cursor);
    tattr->size    = 0;
    tattr->series  = (lql_tuple2_t *) malloc(elems * sizeof(lql_tuple2_t));

    k = 0;
    for (; k<elems && tattr->series != NULL; k+=1)
    {
      tattr->size                   += 1;
      tattr->series[k].fst           = zmq_recvmsg_copydouble__(cursor);
      tattr->series[k].snd           = zmq_recvmsg_copylqlvalue__(cursor);
      tattr->series[k].fst_finalizer = free;
      tattr->series[k].snd_finalizer = (finalizer_f) leela_lql_value_free__;
      if (tattr->series[k].fst == NULL || tattr->series[k].snd == NULL)
      { break; }
    }
    if (k == elems)
    { return(tattr); }
  }

  leela_lql_tattr_free(tattr);
  return(NULL);
}

lql_kattr_t *leela_lql_fetch_kattr (lql_cursor_t *cursor)
{
  lql_kattr_t *kattr;
  if (cursor->row != LQL_KATTR_MSG)
  { return(NULL); }

  kattr = (lql_kattr_t *) malloc(sizeof(lql_kattr_t));
  if (kattr != NULL)
  {
    kattr->guid    = zmq_recvmsg_copystr__(cursor);
    kattr->name    = zmq_recvmsg_copystr__(cursor);
    kattr->value   = zmq_recvmsg_copylqlvalue__(cursor);
    if (kattr->guid == NULL || kattr->name == NULL || kattr->value == NULL)
    {
      leela_lql_kattr_free(kattr);
      kattr = NULL;
    }
  }

  return(kattr);
}

lql_stat_t *leela_lql_fetch_stat (lql_cursor_t *cursor)
{
  uint32_t k;
  uint32_t elems;
  lql_stat_t *stat;
  if (cursor->row != LQL_STAT_MSG)
  { return(NULL); }

  stat = (lql_stat_t *) malloc(sizeof(lql_stat_t));
  if (stat != NULL)
  {
    elems = cursor->elems[1] / 2;
    stat->size  = 0;
    stat->attrs = (lql_tuple2_t *) malloc(elems * sizeof(lql_tuple2_t));

    k = 0;
    for (; k<elems && stat->attrs != NULL; k+=1)
    {
      stat->size                  += 1;
      stat->attrs[k].fst           = (void *) zmq_recvmsg_copystr__(cursor);
      stat->attrs[k].snd           = (void *) zmq_recvmsg_copystr__(cursor);
      stat->attrs[k].fst_finalizer = free;
      stat->attrs[k].snd_finalizer = free;
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

lql_path_t *leela_lql_fetch_path (lql_cursor_t *cursor)
{
  uint32_t k;
  uint32_t elems;
  lql_path_t *path;
  if (cursor->row != LQL_PATH_MSG)
  { return(NULL); }

  path = (lql_path_t *) malloc(sizeof(lql_path_t));
  if (path != NULL)
  {
    elems          = cursor->elems[1] / 2;
    path->size     = 0;
    path->entries  = (lql_tuple2_t *) malloc(elems * sizeof(lql_tuple2_t));

    k = 0;
    for (; k<elems && path->entries != NULL; k+=1)
    {
      path->size                    += 1;
      path->entries[k].fst           = zmq_recvmsg_copystr__(cursor);
      path->entries[k].snd           = zmq_recvmsg_copystr__(cursor);
      path->entries[k].fst_finalizer = free;
      path->entries[k].snd_finalizer = free;
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

lql_fail_t *leela_lql_fetch_fail (lql_cursor_t *cursor)
{
  lql_fail_t *fail;
  if (cursor->row != LQL_FAIL_MSG)
  { return(NULL); }

  fail = (lql_fail_t *) malloc(sizeof(lql_fail_t));
  if (fail != NULL)
  {
    zmq_recvmsg_uint32__(cursor, &(fail->code));
    fail->message = zmq_recvmsg_copystr__(cursor);
    if (fail->message == NULL)
    {
      leela_lql_fail_free(fail);
      fail = NULL;
    }
  }
  return(fail);
}

void leela_lql_name_free (lql_name_t *name)
{
  if (name != NULL)
  {
    free(name->user);
    free(name->tree);
    free(name->kind);
    free(name->name);
    free(name->guid);
    free(name);
  }
}

void leela_lql_stat_free (lql_stat_t *stat)
{
  int k;
  if (stat == NULL)
  { return; }

  for (k=0; k<stat->size; k+=1)
  { leela_lql_tuple2_free_members__(&stat->attrs[k]); }
  free(stat->attrs);
  free(stat);
}

void leela_lql_path_free (lql_path_t *path)
{
  int k;
  if (path == NULL)
  { return; }

  for (k=0; k<path->size; k+=1)
  { leela_lql_tuple2_free_members__(& path->entries[k]); }
  free(path->entries);
  free(path);
}

void leela_lql_fail_free (lql_fail_t *fail)
{
  if (fail != NULL)
  {
    free(fail->message);
    free(fail);
  }
}

void leela_lql_nattr_free (lql_nattr_t *nattr)
{
  int k;
  if (nattr != NULL)
  {
    free(nattr->guid);
    if (nattr->names != NULL)
    {
      for (k=0; k<nattr->size; k+=1)
      { free(nattr->names[k]); }
      free(nattr->names);
    }
    free(nattr);
  }
}

void leela_lql_kattr_free (lql_kattr_t *kattr)
{
  if (kattr != NULL)
  {
    free(kattr->guid);
    free(kattr->name);
    leela_lql_value_free__(kattr->value);
    free(kattr);
  }
}

void leela_lql_tattr_free (lql_tattr_t *tattr)
{
  int k;
  if (tattr != NULL)
  {
    free(tattr->guid);
    free(tattr->name);
    if (tattr->series != NULL)
    {
      for (k=0; k<tattr->size; k+=1)
      { leela_lql_tuple2_free_members__(tattr->series + k); }
      free(tattr->series);
    }
    free(tattr);
  }
}

void leela_lql_cursor_free (lql_cursor_t *cursor)
{ leela_lql_cursor_close(cursor); }

leela_status leela_lql_cursor_close (lql_cursor_t *cursor)
{
  leela_status rc = LEELA_ERROR;
  if (cursor != NULL)
  {
    rc = LEELA_OK;
    if (cursor->socket != NULL && cursor->channel != NULL)
    {
      if (zmq_sendmsg_str__(cursor, "close", cursor->channel, NULL) != -1)
      { zmq_recvmsg_done__(cursor); }
    }
    free(cursor->channel);
    free(cursor->username);
    leela_signature_destroy(cursor->sig);
    zmq_msg_close(&cursor->buffer);
    if (cursor->socket != NULL)
    { zmq_close(cursor->socket); }
    free(cursor);
  }
  return(rc);
}

leela_random_t *leela_random (lql_context_t *ctx)
{ return(ctx != NULL ? ctx->random : NULL); }

void leela_lql_context_free (lql_context_t *ctx)
{ leela_lql_context_close(ctx); }

leela_status leela_lql_context_close (lql_context_t *ctx)
{
  if (ctx != NULL)
  {
    if (ctx->naming != NULL)
    { leela_naming_destroy(ctx->naming); }
    if (ctx->zmqctx != NULL)
    { zmq_ctx_destroy(ctx->zmqctx); }
    if (ctx->random != NULL)
    { leela_random_close(ctx->random); }
    leela_signature_destroy(ctx->sig);
    free(ctx->username);
    pthread_mutex_destroy(&ctx->mutex);
    free(ctx);
    return(LEELA_OK);
  }
  return(LEELA_ERROR);
}
