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

#ifndef __leela_lql_h__
#define __leela_lql_h__

#include <zmq.h>
#include <stdbool.h>
#include "leela/status.h"
#include "leela/endpoint.h"

#define LQL_DEFAULT_TIMEOUT 60

typedef struct lql_cursor_t lql_cursor_t;
typedef struct lql_context_t lql_context_t;

enum lql_row_type {NAME, PATH};

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
} lql_name_t;

/*! Initializes the leela context. You should call this only once and
 *  share it in the program. It is ok, though unecessary, to have
 *  multiple contexts.
 *
 *  \param zookeeper The endpoint of the zookeeper to connect. This is
 *  used to discover the instances of warpdrive to use.
 *
 *  \param path The path to look for warpdrive instances (usually
 *  /naming/warpdrive);
 *  
 *  \return * NULL     : an error has ocurred;
 *          * otherwise: the context has been sucessfully initialized;
 */
lql_context_t *leela_lql_context_init(const leela_endpoint_t *zookeeper, const char *path);

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

#endif
