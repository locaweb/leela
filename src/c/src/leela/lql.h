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
 *  shared it in the program. It is ok, though unecessary, to have
 *  multiple contexts.
 *
 *  \param zookeeper The endpoint of the zookeeper to connect. This is
 *  used to discover the instances of warpdrive to use.
 *  
 *  \return * NULL     : an error has ocurred;
 *          * otherwise: the context has been sucessfully initialized;
 */
struct lql_context_t *lql_context_init(const leela_endpoint_t *zookeeper);

/*! Creates a new cursor.  This selects one available warpdrive
 *  instance to connect to. The actual load balancing algorithm is RR.
 *
 *  Once you get a cursor you may use it only once, i.e., only one
 *  call to `lql_cursor_execute'.
 *
 *  \param ctx The context to use;
 *
 *  \return * NULL     : an error has ocurred;
 *          * otherwise: the cursor has been sucessfully initialized;
 */
struct lql_cursor_t *lql_cursor_init(lql_context_t *ctx);

/*! Executes a query. Make sure you invoke `lql_cursor_next';
 *
 *  \param cursor A valid cursor to use;
 *  \param query The lql query to execute;
 *
 *  \return * LEELA_OK      : success;
 *          * LEELA_ERROR   : any error has ocurred;
 *          * LEELA_BAD_ARGS: the cursor is not valid;
 */
leela_status lql_cursor_execute(lql_cursor_t *cursor, const char *query);

/*! Retrieves the next row out of a cursor;
 *
 *  \param cursor A valid cursor to use;
 *  
 *  \param timeout_in_ms The maximum amount of time (in milliseconds)
 *         to wait for an answer from the server;
 *
 *  \return 
 */
leela_status lql_cursor_next(lql_cursor_t *cursor, int timeout_in_ms);

/*! Terminates a cursor. Remember to always call this function after
 *  you are done iterating.
 *
 *  \param cursor The cursor to close;
 *  
 *  \param nowait * true : Let the server cleans up its resources
 *                         asynchronously. This may return faster at
 *                         the server's expenses;
 *                * false: Waits for the server before returning
 *                         to the caller;
 */
leela_status lql_cursor_close(lql_cursor_t *cursor, bool nowait);

/*! Terminates the context. This may block if there are outstanding
 *  open cursors. Make sure to close them all or this may never
 *  return;
 *
 *  \param ctx The context to close;
 *
 *  \return * LEELA_OK   : success;
 *          * LEELA_ERROR: could not close the context;
 */
leela_status lql_context_close(lql_context_t *ctx);

#endif
