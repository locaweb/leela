// lql_test.cpp
#include <iostream>
#include <string>
#include <cstdlib>
#include <ctime>
#include "UnitTest++.h"
#include <unistd.h>
extern "C"{
#include "lql.h"
}
using namespace std;

static const char alphanum[] =
"0123456789"
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
"abcdefghijklmnopqrstuvwxyz";

int stringLength = sizeof(alphanum) - 1;

void genRandom(char *str){
    srand(time(0));
    for(int z=0; z < 6; z++)
        str[z] = alphanum[rand() % stringLength];
}

TEST(TestDatabaseIsEmpty)
{
    const char *query_matrix[] = {"using (test_database) path (%s);"};


    int major, minor, patch;
    zmq_version (&major, &minor, &patch);
    printf ("Current 0MQ version is %d.%d.%d\n", major, minor, patch);

    row_t *row = NULL;
    context_t *ctx = leela_context_init();
    CHECK(ctx != NULL);
    cursor_t *cur = leela_cursor_init(ctx, "tcp://warp0017.locaweb.com.br:4080");
    CHECK(cur != NULL);

    int len = 6;
    char *datacenter = (char *) malloc(sizeof(char)*(len + 1));
    CHECK(datacenter != NULL);
    genRandom(datacenter);
    datacenter[len] = '\0';

    int i;
    for (i = 0; i < 1; i++){
        int qlen = strlen(query_matrix[i]) + len + 1;
        char *query = (char *)malloc(sizeof(char) * qlen);
        CHECK(query != NULL);

        sprintf(query, query_matrix[i], datacenter);
        CHECK(leela_lql_execute(cur, query) != -1);

        row = (row_t *)malloc(sizeof(row_t));
        CHECK(row != NULL);
        CHECK(leela_cursor_next(cur, row) != -1);

        if(query != NULL){
            free(query);
            query = NULL;
        }
        if(row != NULL){
            free(row);
            row = NULL;
        }
    }

    if(datacenter != NULL){
        free(datacenter);
        datacenter = NULL;
    }
    CHECK(leela_cursor_close(cur, 1) != -1);
    CHECK(leela_context_close(ctx) != -1);
}

TEST(TestMakePath)
{
    const char *query_matrix[] = {"using (test_database) make (%s)\nmake (hm6177);",
        "using (test_database) make (%s) -[machine]> (hm6177);",
        "using (test_database) path (%s);",
        "using (test_database) name %s;" };
    row_t *row = NULL;
    context_t *ctx = leela_context_init();
    CHECK(ctx != NULL);

    int len = 6;
    char *datacenter = (char *) malloc(sizeof(char)*(len + 1));
    CHECK(datacenter != NULL);
    genRandom(datacenter);
    datacenter[len] = '\0';
    char *name;

    int i;
    for (i = 0; i < 4; i++){
        int qlen;
        row = (row_t *)malloc(sizeof(row_t));
        CHECK(row != NULL);

        if (strncmp(query_matrix[i], "using (test_database) name", 26) == 0)
            qlen = strlen(query_matrix[i]) + strlen(name);
        else
            qlen = strlen(query_matrix[i]) + strlen(datacenter);

        char *query = (char *)malloc(sizeof(char) * qlen);
        CHECK(query != NULL);
        cursor_t *cur = leela_cursor_init(ctx, "tcp://warp0017.locaweb.com.br:4080");
        CHECK(cur != NULL);

        if (strncmp(query_matrix[i], "using (test_database) name", 26) == 0)
            sprintf(query, query_matrix[i], name);
        else
            sprintf(query, query_matrix[i], datacenter);

        CHECK(leela_lql_execute(cur, query) != -1);

        CHECK(leela_cursor_next(cur, row) != -1);
        CHECK(row != NULL);
        if (row->row_type == PATH){
            printf("[PRG DEBUG] GUID : %s\n", row->path.guid);
            printf("[PRG DEBUG] LABL : %s\n", row->path.label);

            name = (char *)malloc(strlen(row->path.guid) + 1);
            CHECK(name != NULL);
            strncpy(name, row->path.guid, strlen(row->path.guid));
            name[strlen(row->path.guid)] = '\0';

            if(row->path.label != NULL){
                free(row->path.label);
                row->path.label = NULL;
            }
            if(row->path.guid != NULL){
                free(row->path.guid);
                row->path.guid = NULL;
            }
        }
        if (row->row_type == NAME){
            printf("[PRG DEBUG] USER : %s\n", row->name.user);
            printf("[PRG DEBUG] TREE : %s\n", row->name.tree);
            printf("[PRG DEBUG] NAME : %s\n", row->name.name);

            if(row->name.user != NULL){
                free(row->name.user);
                row->name.user = NULL;
            }
            if(row->name.tree != NULL){
                free(row->name.tree);
                row->name.tree = NULL;
            }
            if(row->name.name != NULL){
                free(row->name.name);
                row->name.name = NULL;
            }
        }
        CHECK(leela_cursor_close(cur, 0) != -1);
        if(query != NULL){
            free(query);
            query = NULL;
        }
        if(row != NULL){
            free(row);
            row = NULL;
        }
    }

    if(name != NULL){
        free(name);
        name = NULL;
    }
    if(datacenter != NULL){
        free(datacenter);
        datacenter = NULL;
    }
    CHECK(leela_context_close(ctx) != -1);
}

int main()
{
    return UnitTest::RunAllTests();
}
