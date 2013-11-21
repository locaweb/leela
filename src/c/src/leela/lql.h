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

enum Type {NAME, PATH, END};

typedef struct path_t_
{
    char           *label;
    char           *guid;
} path_t;

typedef struct name_t_
{
    char           *user;
    char           *tree;
    char           *name;
} name_t;

typedef struct row_t_
{
    enum Type row_type;
    union
    {
        path_t path;
        name_t name;
    };
} row_t;

struct context_t *leela_context_init();

struct cursor_t *leela_cursor_init(struct context_t *ctx, const char *endpoint);

int leela_lql_execute(struct cursor_t *cur, const char * query);

int leela_cursor_next(struct cursor_t *cur, row_t *row);

int leela_cursor_close(struct cursor_t *cur, int nowait);

int leela_context_close(struct context_t *ctx);

#endif //__lql_h__
