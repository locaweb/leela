#include "lql.h"

#define DEBUG 1

struct context_t    { void * ctx; };
struct cursor_t     { void * cur; };
static char * channel;
static int sock_closed = 0;
static int res = 0;
static int timeout = -1;

void leela_lql_debug(const char *prefix, int size, const char *ss){
    if (DEBUG){ fprintf(stderr, "[LIB DEBUG] %s %.*s\n", prefix, size, ss); }
}

int leela_lql_send(struct cursor_t *cur, const char *s, int flags){
    size_t size = -1;
    if (!sock_closed){
        leela_lql_debug(">", strlen(s), s);
        size = zmq_send(cur->cur, s, strlen(s), flags);
        if (size == -1) { goto handle_error; }
    }
    return(size);

handle_error:
    sock_closed = 1;
    return(-1);
}

const char *leela_lql_auth(void){
    const char *key = "usertest:0:0 0";
    return(key);
}

size_t get_msg(struct cursor_t *cur, zmq_msg_t *message){
    size_t size = -1;

    if (zmq_msg_init (message) == -1){ goto handle_error; }
    if (zmq_setsockopt(cur->cur, ZMQ_RCVTIMEO, &timeout, sizeof(timeout)) == -1){ goto handle_error; }

    size = zmq_msg_recv (message, cur->cur, 0);
    if (size == -1){ return(EXIT_SUCCESS); }

    return(size);

handle_error:
    sock_closed = 1;
    return(-1);
}

size_t get_data(struct cursor_t *cur, zmq_msg_t *message, char **msg){
    size_t size = -1;

    if (zmq_msg_close(message) == -1){ goto handle_error; }

    size = get_msg(cur, message);
    *msg = zmq_msg_data(message);
    leela_lql_debug("<", size, *msg);

    return(size);

handle_error:
    sock_closed = 1;
    return(-1);
}

int set_field(struct cursor_t *cur, zmq_msg_t *message, char **msg, char **field){
    size_t size = get_data(cur, message, msg);

    *field = (char *)malloc(sizeof(char)*(size + 1));

    if(*field == NULL){ goto handle_error; }
    strncpy(*field, *msg, size);
    (*field)[size] = '\0';

    return(EXIT_SUCCESS);

handle_error:
    sock_closed = 1;
    if(*field != NULL){
        free(*field);
        *field = NULL;
    }
    return(-1);
}

struct context_t *leela_context_init(){
    struct context_t *ctx = (struct context_t *)malloc(sizeof(struct context_t));
    if(ctx == NULL){ goto handle_error; }

    ctx->ctx = zmq_ctx_new();

    if(ctx->ctx == NULL){ goto handle_error; }
    return(ctx);

handle_error:
    if(ctx != NULL){
        free(ctx);
        ctx = NULL;
    }
    return(NULL);
}

struct cursor_t *leela_cursor_init(struct context_t *ctx, const char *endpoint){
    struct cursor_t *cur =  (struct cursor_t *)malloc(sizeof(struct cursor_t));
    if(cur == NULL){ goto handle_error; }

    cur->cur = zmq_socket (ctx->ctx, ZMQ_REQ);
    if(cur->cur == NULL){
        sock_closed = 1;
        goto handle_error;
    }
    sock_closed = 0;

    if (zmq_connect(cur->cur, endpoint) == -1){
        sock_closed = 1;
        goto handle_error;
    }
    return(cur);

handle_error:
    if(cur != NULL){
        free(cur);
        cur = NULL;
    }
    sock_closed = 1;
    return(NULL);
}

int leela_lql_execute(struct cursor_t *cur, const char * query){
    size_t size = 0;
    int more;
    size_t more_size = sizeof (more);
    char *msg  = NULL;
    zmq_msg_t message;

    if (leela_lql_send(cur, leela_lql_auth(), ZMQ_SNDMORE) < 0) { goto handle_error; }
    if (leela_lql_send(cur, "begin", ZMQ_SNDMORE) < 0) { goto handle_error; }
    if (leela_lql_send(cur, query, 0) < 0) { goto handle_error; }

    size = get_msg(cur, &message);
    msg  = zmq_msg_data(&message);
    msg[size] = '\0';
    leela_lql_debug("<", size, msg);

    if (strncmp(msg, "done", size) != 0){
        if (strncmp(msg, "fail", size) == 0){
            do{
                size = zmq_msg_recv (&message, cur->cur, 0);
                if (size == -1){ goto handle_error; }
                msg  = zmq_msg_data(&message);
                leela_lql_debug("<", size, msg);

                if (zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size) == -1) { goto handle_error; }

            }while(more);
            sock_closed = 1;
        }
        else { goto handle_error; }
    }
    else{
        if (channel == NULL){
            if (zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size) == -1){ goto handle_error; }

            if(more){
                size = zmq_msg_recv (&message, cur->cur, 0);
                if (size == -1){ goto handle_error; }

                channel = (char *)malloc(sizeof(char)*(size + 1));
                if(channel == NULL){ goto handle_error; }
                strncpy(channel, zmq_msg_data(&message), size);
                channel[size] = '\0';
            }
            else { goto handle_error; }
        }
        leela_lql_debug("<", size, channel);
        return(EXIT_SUCCESS);
    }

    if (zmq_msg_close(&message) == -1){ goto handle_error; }
    return(EXIT_SUCCESS);

handle_error:
    sock_closed = 1;
    if(channel != NULL){
        free(channel);
        channel = NULL;
    }
    return(-1);
}

int leela_next(struct cursor_t *cur, row_t *row){
    size_t size = 0;
    int more;
    size_t more_size = sizeof (more);
    char *msg  = NULL;
    zmq_msg_t message;
    char *length = NULL;

    do{
        size = get_msg(cur, &message);
        msg = zmq_msg_data(&message);
        leela_lql_debug("<", size, msg);

        if (zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size) == -1){ goto handle_error; }

        if (strncmp(msg, "done", size) == 0){
            if (!more){
                row->row_type = END;
                res = EXIT_SUCCESS;
            }
            else{
                if (zmq_msg_close(&message) == -1){ goto handle_error; }
                res = leela_next(cur, row);
                if (res == -1){ goto handle_error; }

                more = 0;
                return(res);
            }
        }
        else if (strncmp(msg, "item", size) == 0){
            if (zmq_msg_close(&message) == -1){ goto handle_error; }

            res = leela_next(cur, row);
            if (res == -1){ goto handle_error; }

            return(res);
        }
        else if (strncmp(msg, "list", size) == 0){
            if(set_field(cur, &message, &msg, &length) == -1){ goto handle_error; }

            if (zmq_msg_close(&message) == -1){ goto handle_error; }

            res = leela_next(cur, row);
            if (res == -1){ goto handle_error; }
            res += atoi(length);
            if(length != NULL){
                free(length);
                length = NULL;
            }
            return(res);
        }
        else if (strncmp(msg, "path", size) == 0){
            row->row_type = PATH;
            if(set_field(cur, &message, &msg, &length) == -1){ goto handle_error; }
            res += (atoi(length) / 2 + atoi(length) % 2);

            if(set_field(cur, &message, &msg, &(row->path.label)) == -1){ goto handle_error; }
            if(set_field(cur, &message, &msg, &(row->path.guid)) == -1){ goto handle_error; }
            res -= 1;
            more = 0;
            if(length != NULL){
                free(length);
                length = NULL;
            }
            if (zmq_msg_close(&message) == -1){ goto handle_error; }
            return(res);
        }
        else if (strncmp(msg, "name", size) == 0){
            row->row_type = NAME;
            if(set_field(cur, &message, &msg, &row->name.user) == -1){ goto handle_error; }
            if(set_field(cur, &message, &msg, &row->name.tree) == -1){ goto handle_error; }
            if(set_field(cur, &message, &msg, &row->name.name) == -1){ goto handle_error; }
            more = 0;
            if (zmq_msg_close(&message) == -1){ goto handle_error; }
            return(res);
        }
        else if (strncmp(msg, "fail", size) == 0){
            do{
                size = get_data(cur, &message, &msg);
                zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size);
            }while(more);

            goto handle_error;
        }
        else{
            goto handle_error;
        }

        if (zmq_getsockopt (cur->cur, ZMQ_RCVMORE, &more, &more_size) == -1){
            goto handle_error;
        }

    }while(more);

    if (zmq_msg_close(&message) == -1){ goto handle_error; }
    return(res);

handle_error:
    sock_closed = 1;
    if(length != NULL){
        free(length);
        length = NULL;
    }
    return(-1);
}

int leela_cursor_next(struct cursor_t *cur, row_t *row, int tmout){

    timeout = tmout;
    if(res == 0){
        if (leela_lql_send(cur, leela_lql_auth(), ZMQ_SNDMORE) < 0){ goto handle_error; }
        if (leela_lql_send(cur, "fetch", ZMQ_SNDMORE) < 0){ goto handle_error; }
        if (leela_lql_send(cur, channel, 0) < 0){ goto handle_error; }
    }

    return(leela_next(cur, row));

handle_error:
    sock_closed = 1;
    return(-1);
}

int leela_cursor_close(struct cursor_t *cur, int nowait){
    if (!sock_closed){
        if (leela_lql_send(cur, leela_lql_auth(), ZMQ_SNDMORE) < 0){ goto handle_error; }
        if (leela_lql_send(cur, "close", ZMQ_SNDMORE) < 0){ goto handle_error; }
        if(nowait){
            if (leela_lql_send(cur, channel, ZMQ_SNDMORE) < 0){ goto handle_error; }
            if (leela_lql_send(cur, "nowait", 0) < 0){ goto handle_error; }
        }
        else{
            if (leela_lql_send(cur, channel, 0) < 0){ goto handle_error; }
        }

        if (zmq_close(cur->cur) == -1){ goto handle_error; }
    }

    sock_closed = 1;
    if (channel != NULL){
        free(channel);
        channel = NULL;
    }

    if (cur != NULL){
        free(cur);
        cur = NULL;
    }
    return(EXIT_SUCCESS);

handle_error:
    sock_closed = 1;
    return(-1);
}

int leela_context_close(struct context_t *ctx){
    if (zmq_ctx_destroy(ctx->ctx) == -1){ goto handle_error; }
    if (ctx != NULL){
        free(ctx);
        ctx = NULL;
    }
    return(EXIT_SUCCESS);

handle_error:
    sock_closed = 1;
    if (ctx != NULL){
        free(ctx);
        ctx = NULL;
    }
    return(-1);
}
