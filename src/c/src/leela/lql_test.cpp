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

void create(char **query, const char *orig, const char *label, const char *dest){
    int dl = 32 + (2*(strlen(orig) + 1)) + (strlen(label) + 1) + (2*(strlen(dest) + 1));
    int dq = dl;
    if (*query != NULL)
        dq += strlen(*query) + 1;

    char *tmp = (char *)malloc(sizeof(char) * dq);
    if (tmp != NULL){
        if(*query != NULL){
            strncpy(tmp, *query, strlen(*query));
            tmp[strlen(*query)] = '\0';
            free(*query);
        }
        else{
            strncpy(tmp, "", 1);
            tmp[1] = '\0';
        }
        *query = tmp;
    }
    else{
        free(*query);
        free(tmp);
        exit(1);
    }

    tmp = (char *)malloc(sizeof(char) * dl);
    const char * str = "make (%s)\nmake (%s)\nmake (%s) -[%s]> (%s)\n";
    sprintf(tmp, str, orig, dest, orig, label, dest);
    strncat(*query, tmp, dl);
    free(tmp);
}

void gen_ni_name(char **mac, int num, int k){
    int x = num * 8 + k;
    char *temp = (char *)malloc(sizeof(char)*3);
    *mac = (char *)malloc(sizeof(char)*19);
    strncpy(*mac, "", 1);
    (*mac)[1] = '\0';

    while(x > 0){
        sprintf(temp, "%02x", x%256);
        strncat(*mac, temp, 2);
        strncat(*mac, ":", 1);
        x /= 256;
    }
    while(strlen(*mac) < 16){
        strncat(*mac, "00", 2);
        strncat(*mac, ":", 1);
    }
    (*mac)[strlen(*mac) - 1] = '\0';
    free(temp);
}

char * create_mc(int x){
    char *query = NULL;

    char *machine = (char *)malloc(sizeof(char)*(strlen("hm0000") + 1));
    sprintf(machine, "hm%04d", x);

    char *sw = (char *)malloc(sizeof(char)*(strlen("sw0000") + 1));
    sprintf(sw, "sw%04d", (x*8)/64);

    create(&query, "ITA", "machine", machine);
    create(&query, "ITA", "switch", sw);

    int k;
    for(k = 0; k < 8; k++){
        char *mac = NULL;
        gen_ni_name(&mac, x, k);

        create(&query, machine, "nic", mac);
        create(&query, mac, "link", sw);

        free(mac);
    }

    char *res = (char *)malloc(sizeof(char)*(strlen("using (test_database) ;") + strlen(query) + 2));
    query[strlen(query) - 1] = '\0';
    sprintf(res, "using (test_database) %s;", query);
    free(machine);
    free(sw);
    free(query);
    return(res);
}

struct TestSetUp
{
    TestSetUp()
    {
        context_t *ctx = leela_context_init();

        int x;
        row_t *row = NULL;
        for(x = 0; x < 1; x++){
            cursor_t *cur = leela_cursor_init(ctx, "tcp://warp0017.locaweb.com.br:4080");
            row = (row_t *)malloc(sizeof(row_t));
            char *query = create_mc(x);

            leela_lql_execute(cur, query);
            leela_cursor_next(cur, row);

            leela_cursor_close(cur, 1);
            if(query != NULL){
                free(query);
                query = NULL;
            }
            if(row != NULL){
                free(row);
                row = NULL;
            }
        }
        leela_context_close(ctx);
    }
};

TEST(TestDatabaseIsEmpty)
{
    const char *query_matrix[] = {"using (test_database) path (%s);"};

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

        int res = 0;
        do{
            res = leela_cursor_next(cur, row);
            CHECK(res != -1);
            CHECK(row != NULL);
            if (row->row_type == PATH){
                printf("[PRG DEBUG] LABL : %s\n", row->path.label);
                printf("[PRG DEBUG] GUID : %s\n", row->path.guid);

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
        }while(res > 0);
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

TEST_FIXTURE(TestSetUp, TestItemAndList)
{
    row_t *row = (row_t *)malloc(sizeof(row_t));
    context_t *ctx = leela_context_init();
    CHECK(ctx != NULL);
    cursor_t *cur = leela_cursor_init(ctx, "tcp://warp0017.locaweb.com.br:4080");
    CHECK(cur != NULL);

    CHECK(leela_lql_execute(cur, "using (test_database) path (hm0000);") != -1);

    int res = 0;
    do{
        res = leela_cursor_next(cur, row);
        CHECK(res != -1);
        CHECK(row != NULL);
        if (row->row_type == PATH){
            printf("[PRG DEBUG] LABL : %s\n", row->path.label);
            printf("[PRG DEBUG] GUID : %s\n", row->path.guid);

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
    }while(res > 0);

    if(row != NULL){
        free(row);
        row = NULL;
    }

    CHECK(leela_cursor_close(cur, 1) != -1);
    CHECK(leela_context_close(ctx) != -1);
}

int main()
{
    return UnitTest::RunAllTests();
}
