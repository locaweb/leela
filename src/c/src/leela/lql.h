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
#include <stdlib.h>

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
struct row_t
{
    enum Type row_type;
    union
    {
        struct path_t;
        struct name_t;
    };
};

/*
 * It returns a thread-safe context to be used
 * to connects to the Pool of Leela Servers
 * Returns:
 *      Context;
 * Error:
 *      Returns NULL and errno are setted.
 */
void * lql_context();

/*
 * Given a context and an endpoint, it returns
 * a socket that will be used to connect
 * to the Pool of Leela Servers.
 * Returns:
 *      Socket;
 * Error:
 *      Returns NULL and errno are setted.
 */
void * lql_open(void *ctx, const char *endpoint);

/*
 * Given a socket and a query, it returns
 * a cursor that will be used to fetch data
 * from the Pool of Leela Servers.
 * Returns:
 *      Cursor;
 * Error:
 *      Returns NULL and errno are setted.
 */
void * lql_execute(void *socket, const char * query);

/*
 * Fetch in an iterative way, the data returned by
 * the Pool of Leela Servers. The data is returned
 * back through the row argument as a structure
 * related to the data that it contains.
 * Returns:
 *      Zero            - Success;
 *      Negative Number - Error Code;
 */
int lql_cursor_next(void *cursor, struct row_t *row);

/*
 * Closes the socket associated with the cursor
 * and clean the linked list used to store data.
 * Returns:
 *      Zero            - Success;
 *      Negative Number - Error Code;
 */
int lql_cursor_close(void *cursor);

/*
 * Closes the context and free all process
 * used for communication purpose.
 * Returns:
 *      Zero            - Success;
 *      Negative Number - Error Code;
 */
int lql_context_close(void *context);

#endif //__lql_h__
