#include "lql.h"

struct context_t    { void * ctx; };
struct cursor_t     { void * cur; };

const char *auth(void){
    const char *key = "usertest:0:0 0";
    return key;
}

context_t *leela_context_init(){
    context_t *ctx = (context_t *)malloc(sizeof(context_t *));
    ctx->ctx = zmq_ctx_new();

    if (ctx->ctx != NULL)
        return(ctx);

    return(NULL);
}

cursor_t *leela_cursor_init(context_t *ctx, const char *endpoint){
    cursor_t *cur =  (cursor_t *)malloc(sizeof(cursor_t *));
    cur->cur = zmq_socket (ctx, ZMQ_REQ);
    zmq_connect(cur->cur, endpoint);
    return(cur);
}

int leela_lql_execute(cursor_t *cur, const char * query){
    return(0);
}

int leela_cursor_next(cursor_t *cur, row_t *row){
    return(0);
}

int leela_cursor_close(cursor_t *cur){
    int res = zmq_close((reinterpret_cast<cursor_t *>(cur))->cur);
    free(cur);
    return(res);
}

int leela_context_close(void *ctx){
    int res = zmq_ctx_destroy((reinterpret_cast<context_t *>(ctx))->ctx);
    free(ctx);
    return(res);
}
