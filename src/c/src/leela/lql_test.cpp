// lql_test.cpp
#include "lql.h"
#include "UnitTest++.h"

void id_generator(char *s, const int len) {
    for (int i = 0; i < len; ++i) {
        int randomChar = rand()%(26+26+10);
        if (randomChar < 26)
            s[i] = 'a' + randomChar;
        else if (randomChar < 26+26)
            s[i] = 'A' + randomChar - 26;
        else
            s[i] = '0' + randomChar - 26 - 26;
    }
    s[len] = 0;
}

TEST(TestDatabaseIsEmpty)
{
    const char *query_matrix[] = {"using (test_database) path (%s);"};
    row_t *row = NULL;
    context_t *ctx = leela_context_init();
    connection_t *con = leela_connection_init(ctx, "tcp://warp0017.locaweb.com.br:4080");
    int len = 6;
    char *datacenter = (char *) malloc(sizeof(char *)*len);
    
    id_generator(datacenter, len);

    int i, j;
    for (i = 0; i < 1; i++){
        int qlen = strlen(query_matrix[i]) + len + 1;
        char *query = (char *)malloc(sizeof(char *) * qlen);

        sprintf(query, query_matrix[i], datacenter);
        cursor_t *cur = leela_lql_execute(con, query);

        CHECK(leela_cursor_next(cur, row) == 0);

        leela_cursor_close(cur);
        free(query);
    }

    free(datacenter);
    leela_context_close(ctx);
}

TEST(TestMakePath)
{
    const char *query_matrix[] = {"using (test_database) path (%s);",
                                  "using (test_database) make (%s)\nmake (hm6177);",
                                  "using (test_database) make (%s) -[machine]> (hm6177);",
                                  "using (test_database) path (%s);",
                                  "using (test_database) name %s;" };
    row_t *row = NULL;
    context_t *ctx = leela_context_init();
    connection_t *con = leela_connection_init(ctx, "tcp://warp0017.locaweb.com.br:4080");
    int len = 6;
    char *datacenter = (char *) malloc(sizeof(char *)*len);
    
    id_generator(datacenter, len);

    int i, j;
    for (i = 0; i < 5; i++){
        int qlen = strlen(query_matrix[i]) + len + 1;
        char *query = (char *)malloc(sizeof(char *) * qlen);

        sprintf(query, query_matrix[i], datacenter);
        cursor_t *cur = leela_lql_execute(con, query);
        leela_cursor_next(cur, row);
        while (row != NULL){
            leela_cursor_next(cur, row);
        }

        CHECK(true);

        leela_cursor_close(cur);
        free(query);
    }

    free(datacenter);
    leela_context_close(ctx);
}

int main()
{
    return UnitTest::RunAllTests();
}
