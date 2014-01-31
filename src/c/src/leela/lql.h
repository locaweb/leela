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

#ifndef __leela_lql_h__
#define __leela_lql_h__

#include <zmq.h>
#include <stdbool.h>
#include "leela/status.h"
#include "leela/endpoint.h"

#define LQL_DEFAULT_TIMEOUT 60

#ifdef __cplusplus
extern "C" {
#endif

typedef struct lql_cursor_t lql_cursor_t;
typedef struct lql_context_t lql_context_t;

typedef enum
{
  LQL_NAME_MSG,
  LQL_PATH_MSG,
  LQL_STAT_MSG,
  LQL_FAIL_MSG,
  LQL_NATTR_MSG,
  LQL_KATTR_MSG,
} lql_row_type;

typedef enum
{
  LQL_NIL_TYPE    = -1,
  LQL_BOOL_TYPE   = 0,
  LQL_TEXT_TYPE   = 1,
  LQL_INT32_TYPE  = 2,
  LQL_INT64_TYPE  = 3,
  LQL_UINT32_TYPE = 4,
  LQL_UINT64_TYPE = 5,
  LQL_DOUBLE_TYPE = 6
} lql_value_type;

typedef struct
{
  lql_value_type vtype;
  union
  {
    char    *v_str;
    int32_t  v_i32;
    int64_t  v_i64;
    uint32_t v_u32;
    uint64_t v_u64;
    bool     v_bool;
    double   v_double;
  } data;
} lql_value_t;

//! A simple 2-tuple type;
typedef struct
{
  char *fst;
  char *snd;
} lql_tuple2_t;

//! A path entry as defined in warpdrive(1);
typedef struct lql_path_t
{
  int          size;      //!^ The number of path entries;
  lql_tuple2_t *entries;  //!^ The path entries;
} lql_path_t;

//! A name entry as defined in warpdrive(1);
typedef struct
{
  char *user;             //!^ The owner of this node;
  char *tree;             //!^ The namespace of this node;
  char *name;             //!^ The name of this name;
  char *guid;             //!^ The hash that has been requested;
} lql_name_t;

//! An fail entry as defined in warpdrive(1);
typedef struct
{
  uint32_t code;          //!^ The fail code reported;
  char *message;          //!^ The fail message;
} lql_fail_t;

//! The attribute names (n-attr message)
typedef struct
{
  int size;               //!^ The number of entries
  char *guid;             //!^ The node we are referencing
  char **names;           //!^ The attr names
} lql_nattr_t;

//! The attribute name & value (k-attr message)
typedef struct
{
  char *guid;             //!^ The node this attribute is set on
  char *name;             //!^ The name of the attribute
  lql_value_t *value;     //!^ The value
} lql_kattr_t;

//! Information about the cluster
typedef struct
{
  int           size;     //!^ The number of key-value entries;
  lql_tuple2_t *attrs;    //!^ The property (key-value);
} lql_stat_t;

/*! Initializes the leela context. You should call this only once and
 *  share it in the program. It is ok, though unecessary, to have
 *  multiple contexts.
 *
 *  \param warpdrive The endpoint of some machine on the leela cluster
 *  to connect (refer to naming.h#leela_naming_init for more info);
 *
 *  \return * NULL     : an error has ocurred;
 *          * otherwise: the context has been sucessfully initialized;
 */
lql_context_t *leela_lql_context_init(const leela_endpoint_t *const *warpdrive);

/*! Creates a new cursor.  This selects one available warpdrive
 *  instance to connect to. The actual load balancing algorithm is
 *  implementation dependent.
 *
 *  The cursor can be used to execute a single statement. If you need
 *  to perform more queries, you must create another cursor.
 *
 *  \param ctx The context to use;
 *
 *  \param username, secret The credentials to authenticate (must not be NULL);
 *
 *  \param timeout_in_ms The maximum amount of time (in milliseconds)
 *         to wait for an answer from the server. Use (-1) to wait
 *         forever and (0) to use the default (implementation defined)
 *         timeout;
 *
 *  \return * NULL     : an error has ocurred;
 *          * otherwise: the cursor has been sucessfully initialized;
 */
lql_cursor_t *leela_lql_cursor_init(lql_context_t *ctx, const char *username, const char *secret, int timeout_in_ms);

// TODO!
lql_cursor_t *leela_lql_cursor_init2(lql_context_t *ctx, const leela_endpoint_t *endpoint, const char *username, const char *secret, int timeout_in_ms);

/*! Executes a query. To consume the results use leela_cursor_next
 *
 *  \param cursor A valid cursor to use;
 *  \param query The lql query to execute;
 *
 *  \return * LEELA_OK     : success;
 *          * LEELA_ERROR  : any error has ocurred;
 *          * LEELA_BADARGS: the cursor is not valid;
 */
leela_status leela_lql_cursor_execute(lql_cursor_t *cursor, const char *query);

/*! Retrieves the next row out of a cursor;
 *
 *  \param cursor A valid cursor to use;
 *
 *  \return LEELA_OK success;
 *  \return LEELA_EOF there are no more entries;
 *  \return LEELA_TIMEOUT the operation has timed out;
 */
leela_status leela_lql_cursor_next(lql_cursor_t *cursor);

/*! Retrieves the current row type. Notice you *must* invoke
 * `leela_lql_cursor_next' and it *must* return `LEELA_OK' prior
 * calling this function. Return `LEELA_ERROR'
 * if a `fail' message was received.
 */
lql_row_type leela_lql_fetch_type(lql_cursor_t *cursor);

/*! Extracts the name message from the cursor. Use this function only
 *  if the message type is LQL_NAME (refer to leela_lql_fetch_type);
 */
lql_name_t *leela_lql_fetch_name(lql_cursor_t *cursor);

/*! Extracts the path message from the cursor. Use this function only
 *  if the message type is LQL_PATH (refer to leela_lql_fetch_type);
 */
lql_path_t *leela_lql_fetch_path(lql_cursor_t *cursor);

/*! Extracts the stat message from the cursor. Use this function only
 *  if the message type is LQL_STAT (refer to leela_lql_fetch_type);
 */
lql_stat_t *leela_lql_fetch_stat(lql_cursor_t *cursor);

/*! Extracts the error message from the cursor. Use this function only
 *  if the message type is LQL_FAIL_MSG (refer to leela_lql_fetch_type);
 */
lql_fail_t *leela_lql_fetch_fail(lql_cursor_t *cursor);

/*! Extracts the nattr message from the cursor, which contains the
 *  attribute names of a given node. Use this function only if the
 *  message type is LQL_NATTR_MSG (refer to leela_lql_nattr_type).
 */
lql_nattr_t *leela_lql_fetch_nattr(lql_cursor_t *cursor);

/*! Extracts the kattr message from the cursor. This message contains
 *  the attribute value. Use this function only if the message type is
 *  LQL_KATTR_MSG (refer to leela_lql_kattr_type).
 */
lql_kattr_t *leela_lql_fetch_kattr(lql_cursor_t *cursor);

void leela_lql_name_free(lql_name_t *);
void leela_lql_path_free(lql_path_t *);
void leela_lql_stat_free(lql_stat_t *);
void leela_lql_fail_free(lql_fail_t *);
void leela_lql_nattr_free(lql_nattr_t *);
void leela_lql_kattr_free(lql_kattr_t *);

/*! Terminates a cursor. Remember to always call this function after
 *  you are done iterating.
 *
 *  \param cursor The cursor to close;
 */
leela_status leela_lql_cursor_close(lql_cursor_t *cursor);

/*! Terminates the context. This may block if there are outstanding
 *  open cursors. Make sure to close them all or this may never
 *  return;
 *
 *  \param ctx The context to close;
 *
 *  \return * LEELA_OK   : success;
 *          * LEELA_ERROR: could not close the context;
 */
leela_status leela_lql_context_close(lql_context_t *ctx);

#ifdef __cplusplus
}
#endif

#endif
