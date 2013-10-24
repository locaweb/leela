#include "lql.h"

#define DEBUG 1

struct context_t    { void * ctx; };
struct cursor_t     { void * cur; };

void debug(const char *prefix, const char *ss){
    if (DEBUG){
        fprintf(stderr, "[DEBUG] %s %s\n", prefix, ss);
    }
}

int send(cursor_t *cur, const char *s, int flags=0){
    debug(">", s);
    int size = zmq_send(cur->cur, s, strlen(s), flags);
    return (size);
}

char * recv(cursor_t *cur, int flags=0){
    char *msg = (char *)malloc(1);
    msg[0] = 0;
    int total_size = 0;
    while (1) 
    {
        zmq_msg_t message;
        zmq_msg_init (&message);
        int size = zmq_msg_recv (&message, cur->cur, 0);
        if (size == -1)
            return NULL;
        
        total_size += size + 1;
        msg = (char *)realloc(msg, total_size + 1);
        msg = strncat(msg, (const char *)zmq_msg_data (&message), size);
        zmq_msg_close (&message);
        msg[total_size] = 0;
        int more;
        size_t more_size = sizeof (more);
        debug("<", msg);
        msg = strncat(msg, "%", 1);
        zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size);
        if (!more)
            return(msg);
    }
}

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
    cur->cur = zmq_socket (ctx->ctx, ZMQ_REQ);
    zmq_connect(cur->cur, endpoint);
    return(cur);
}

int leela_lql_execute(cursor_t *cur, const char * query){
    int res = 0;
    res = send(cur, auth(), ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = send(cur, "begin", ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = send(cur, query);
    if (res < 0)
        return (res);

    char *msg = recv(cur);
    debug("=", msg);
    const char *done = "done";
    if (strncmp(done, strtok(msg, "%"), strlen(done)) != 0){
        res = -1;
    }
    else{
        static char * channel;
        channel = strtok(NULL, "%");
        debug("==", channel);
        res = 0;
    }

    free(msg);
    return(res);
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
