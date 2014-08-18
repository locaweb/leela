/* Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
 *                    Andre Ferraz <deferraz@terra.com.br>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "lql.h"
#include "status.h"
#include "string.h"
#include "naming.h"
#include "signature.h"

#define DEBUG 1

typedef struct {
  lql_cursor_t *cursor1;
  lql_cursor_t *cursor2;
} lql_meta_cursor_t;

lql_meta_cursor_t *leela_new_meta_cursor(lql_cursor_t *cursor1, lql_cursor_t *cursor2)
{
  lql_meta_cursor_t *meta = NULL;
  meta = malloc(sizeof(lql_meta_cursor_t));
  if (meta == NULL)
  {
    return NULL;
  }
  meta->cursor1 = cursor1;
  meta->cursor2 = cursor2;
  return(meta);
}

leela_status leela_lql_meta_cursor_next (lql_meta_cursor_t *meta)
{
  leela_lql_cursor_next(meta->cursor1);
  leela_lql_cursor_next(meta->cursor2);
}

lql_tattr_t *leela_fetch_meta_cursor(lql_meta_cursor_t *meta, aggr_tattr_f aggr_func)
{
  lql_tattr_t *tattr = (lql_tattr_t *) malloc(sizeof(lql_tattr_t));
  if (tattr == NULL)
  {
    return(NULL);
  }
  int i, j;
  lql_tattr_t *tattr1 = leela_lql_fetch_tattr(meta->cursor1);
  lql_tattr_t *tattr2 = leela_lql_fetch_tattr(meta->cursor2);
  i = 0;
  j = 0;
  int max = tattr1->size > tattr2->size ? tattr1->size : tattr2->size;
  tattr->series = malloc(sizeof(lql_tattr_t) * max);
  if (tattr->series == NULL)
  {
    leela_lql_tattr_free(tattr);
    leela_lql_tattr_free(tattr1);
    leela_lql_tattr_free(tattr2);
    return(NULL);
  }
  tattr->size   = max;
  while ((i < tattr1->size) && (j < tattr2->size))
  {
    double *dval = (double *) tattr1->series[i].fst;
    tattr->series[i].fst = (double *) malloc(sizeof(double));
    if (tattr->series[i].fst == NULL)
    {
      leela_lql_tattr_free(tattr);
      leela_lql_tattr_free(tattr1);
      leela_lql_tattr_free(tattr2);
      return(NULL);
    }
    *(double *)tattr->series[i].fst = *dval;
    tattr->series[i].snd = (lql_value_t *) malloc(sizeof(lql_value_t));
    if (tattr->series[i].snd == NULL)
    {
      leela_lql_tattr_free(tattr);
      leela_lql_tattr_free(tattr1);
      leela_lql_tattr_free(tattr2);
      return(NULL);
    }
    lql_value_t *value1 = (lql_value_t *) tattr1->series[i].snd;
    lql_value_t *value2 = (lql_value_t *) tattr2->series[j].snd;

    lql_value_t *value  = (lql_value_t *)tattr->series[i].snd;
    value->data.v_double = value1->data.v_double + value2->data.v_double;
    value = aggr_func(value1, value2);
    printf("Value1: %f, Value2: %f, Aggregation: %f\n", value1->data.v_double, value2->data.v_double, value->data.v_double);
    /*
    printf("Timeseries: %f, Somatoria %f\n", *dval, value1->data.v_double + value2->data.v_double);
    */
    i++;
    j++;
  }
  leela_lql_tattr_free(tattr1);
  leela_lql_tattr_free(tattr2);
  return(tattr);
}
