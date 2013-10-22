#include "lql.h"

context_t *leela_context_init(){
    context_t *ctx = NULL;
    return(ctx);
}

connection_t *leela_connection_init(context_t *ctx, const char *endpoint){
    connection_t *con = NULL;
    return(con);
}

cursor_t * leela_lql_execute(connection_t *con, const char * query){
    cursor_t * cur = NULL;
    return(cur);
}

int leela_cursor_next(cursor_t *cur, row_t *row){
    return(0);
}

int leela_cursor_close(cursor_t *cur){
    return(0);
}

int leela_context_close(void *ctx){
    return(0);
}
