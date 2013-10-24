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

#ifndef __lql_h__
#define __lql_h__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zmq.h>
#include <assert.h>

struct context_t;
struct cursor_t;

enum Type {NAME, PATH};

/*
 * Single direction linked list
 * to handle the PATH command
 * response. The structure fields
 * are related to the data requested
 * by the user.
 */
typedef struct path_t_
{
    char           *guid;
    char           *label;
    struct path_t_ *next;
} path_t;

/*
 * Single direction linked list
 * to handle the NAME command
 * response. The structure fields
 * are related to the data requested
 * by the user.
 */
typedef struct name_t_
{
    char           *user;
    char           *tree;
    char           *name;
    struct name_t_ *next;
} name_t;

/*
 *
 */
typedef struct row_t_
{
    enum Type row_type;
    union
    {
        path_t path;
        name_t name;
    };
} row_t;

/*
 * It returns a thread-safe context to be used
 * to connects to the Pool of Leela Servers
 * Returns:
 *      Context;
 * Error:
 *      Returns NULL
 */
context_t *leela_context_init();

/*
 * Given a context and an endpoint, it returns
 * a Cursor that will be used to attach
 * to the Pool of Leela Servers.
 * Returns:
 *      Cursor
 * Error:
 *      Returns NULL
 */
cursor_t *leela_cursor_init(context_t *ctx, const char *endpoint);

/*
 * Given a cursor and a query, it starts
 * the query process to fetch data
 * from the Pool of Leela Servers.
 * Returns:
 *      Cursor;
 * Error:
 *       0:            - Success;
 *      <0:            - Error Code;
 */
int leela_lql_execute(cursor_t *cur, const char * query);

/*
 * Fetch in an iterative way, the data returned by
 * the Pool of Leela Servers. The data is returned
 * back through the row argument as a structure
 * related to the data that it contains.
 * Returns:
 *       0:            - Success;
 *      <0:            - Error Code;
 */
int leela_cursor_next(cursor_t *cur, row_t *row);

/*
 * Closes the socket associated with the cursor
 * and clean the linked list used to store data.
 * Returns:
 *      >0:            - Success;
 *       0:            - Finished;
 *      <0:            - Error Code;
 */
int leela_cursor_close(cursor_t *cur);

/*
 * Closes the context and free all process
 * used for communication purpose.
 * Returns:
 *       0:            - Success;
 *      <0:            - Error Code;
 */
int leela_context_close(void *ctx);

#endif //__lql_h__
