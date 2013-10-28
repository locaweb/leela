#include "lql.h"

#define DEBUG 1

struct context_t    { void * ctx; };
struct cursor_t     { void * cur; };
static char * channel = NULL;

void leela_lql_debug(const char *prefix, const char *ss){
    if (DEBUG){
        fprintf(stderr, "[DEBUG] %s %s\n", prefix, ss);
    }
}

int leela_lql_send(struct cursor_t *cur, const char *s, int flags){
    leela_lql_debug(">", s);
    int size = zmq_send(cur->cur, s, strlen(s), flags);
    return (size);
}

char * leela_lql_recv(struct cursor_t *cur, int flags){
    char *msg = (char *)malloc(1);
    msg[0] = 0;
    int total_size = 0;
    while (1){
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
        leela_lql_debug("<", msg);
        msg = strncat(msg, "%", 1);
        zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size);
        if (!more)
            return(msg);
    }
}

const char *leela_lql_auth(void){
    const char *key = "usertest:0:0 0";
    return key;
}

struct context_t *leela_context_init(){
    struct context_t *ctx = (struct context_t *)malloc(sizeof(struct context_t));
    ctx->ctx = zmq_ctx_new();

    if (ctx->ctx != NULL)
        return(ctx);

    return(NULL);
}

struct cursor_t *leela_cursor_init(struct context_t *ctx, const char *endpoint){
    struct cursor_t *cur =  (struct cursor_t *)malloc(sizeof(struct cursor_t));
    cur->cur = zmq_socket (ctx->ctx, ZMQ_REQ);
    zmq_connect(cur->cur, endpoint);
    return(cur);
}

int leela_lql_execute(struct cursor_t *cur, const char * query){
    int res = 0;
    res = leela_lql_send(cur, leela_lql_auth(), ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = leela_lql_send(cur, "begin", ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = leela_lql_send(cur, query, 0);
    if (res < 0)
        return (res);

    char *msg = leela_lql_recv(cur, 0);
    if (strncmp(strtok(msg, "%"), "done", strlen("done")) != 0){
        res = -1;
    }
    else{
        if (channel == NULL)
            channel = strdup(strtok(NULL, "%"));
        leela_lql_debug("==", channel);
        res = 0;
    }

    free(msg);
    return(res);
}

int leela_cursor_next(struct cursor_t *cur, row_t *row){
    int res = 0;
    res = leela_lql_send(cur, leela_lql_auth(), ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = leela_lql_send(cur, "fetch", ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = leela_lql_send(cur, channel, 0);
    if (res < 0)
        return (res);

    char *msg = leela_lql_recv(cur, 0);
    char *data = strdup(strtok(msg, "%"));
    if (strncmp(data, "fail", strlen("fail")) == 0){
        res = -1;
    }
    printf("%s\n", data);
    free(data);
    free(msg);
    return(res);
}

int leela_cursor_close(struct cursor_t *cur, int nowait){
    int res = 0;
    res = leela_lql_send(cur, leela_lql_auth(), ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    res = leela_lql_send(cur, "close", ZMQ_SNDMORE);
    if (res < 0)
        return (res);
    if(nowait){
        res = leela_lql_send(cur, channel, ZMQ_SNDMORE);
        if (res < 0)
            return (res);
        res = leela_lql_send(cur, "nowait", 0);
        if (res < 0)
            return (res);
    }
    else{
        res = leela_lql_send(cur, channel, 0);
        if (res < 0)
            return (res);
    }

    res = zmq_close(cur->cur);
    free(channel);
    channel = NULL;
    free(cur);
    return(res);
}

int leela_context_close(struct context_t *ctx){
    int res = zmq_ctx_destroy(ctx->ctx);
    free(ctx);
    return(res);
}
