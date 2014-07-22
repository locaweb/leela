// Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "leela/lql.h"
#include "leela/status.h"
#include "leela/endpoint.h"

static
const char *__spaces (const char *string)
{
  if (string == NULL)
  { return(NULL); }

  int at = 0;
  while (string[at] != '\0' && string[at] == ' ')
  { at += 1; }
  return(string + at);
}

static
const char *__skip (const char *string, int skip)
{
  if (string == NULL)
  { return(NULL); }
  while (skip-- > 0 && string[0] != '\0')
  { string = string + 1; }
  return(string);
}

static
char *__delimited (const char *string, char begin, char end)
{
  if (string == NULL)
  { return(NULL); }

  int at = 0;
  if (string[at] == begin)
  {
    at += 1;
    while (string[at] != '\0' && string[at] != end)
    { at += 1; }
    at -= 1;
  }
  char *dest = (char *) malloc(at+1);
  strncpy(dest, string+1, at);
  dest[at]   = '\0';
  return(dest);
}

char *__unquote (char *string)
{
  if (string == NULL)
  { return(NULL); }
  int at = 0;
  int of = 0;
  while (string[at] == ' ')
  { at++; }
  if (string[at] == '"')
  {
    for (; string[at+1]!='"'; at += 1)
    { string[of++] = string[at+1]; }
    string[of] = '\0';
  }
  return(string);
}

static
char *__unslash (char *string)
{
  if (string == NULL)
  { return(NULL); }

  int of = 0;
  for (int at=0; string[at]!='\0'; at+=1)
  {
    if (string[at] != '\\')
    { string[of++] = string[at]; }
  }
  return(string);
}

static
char *__get_key (const char *json, const char *key)
{
  const char *begin = __spaces(__skip(strstr(json, key), strlen(key)));
  if (begin == NULL)
  { return(NULL); }

  if (begin[0] == '"')
  { return(__delimited(begin, '"', '"')); }
  else if (begin[0] == '[')
  { return(__delimited(begin, '[', ']')); }
  else
  { return(NULL); }
}

static
leela_endpoint_t **__get_endpoint (const char *json, const char *key)
{
  int size       = 16;
  leela_endpoint_t **endpoints = (leela_endpoint_t **) malloc(sizeof(leela_endpoint_t *) * size);
  char *endpoint = __get_key(json, key);
  int index      = 0;
  int begin      = 0;

  for (int at=0; at < size; at+=1)
  { endpoints[at] = NULL; }
  for (int at=0; endpoint[at] != '\0'; at+=1)
  {
    if (endpoint[at] == ',')
    {
      endpoint[at] = '\0';
      if (index < size-1)
      { endpoints[index++] = leela_endpoint_load(__unquote(endpoint + begin)); }
      begin = at + 1;
    }
  }
  if (index < size-1)
  { endpoints[index++] = leela_endpoint_load(__unquote(endpoint + begin)); }
  free(endpoint);
  return(endpoints);
}

static
char *__get_string (const char *json, const char *key)
{
  char *string = __get_key(json, key);
  return(__unquote(string));
}

static
void __free_endpoints (leela_endpoint_t **endpoints)
{
  for (int at=0; endpoints[at] != NULL; at+=1)
  { leela_endpoint_free(endpoints[at]); }
  free(endpoints);
}

static
char *__readline (char *buffer, size_t size)
{
  int k=0;
  buffer[size-1] = '\0';
  for (k=0; k<size-1; k+=1)
  {
    buffer[k] = '\0';
    int c = fgetc(stdin);
    if (c == EOF)
    { break; }

    buffer[k] = c;
    if (buffer[k] == '\n')
    { buffer[k] = '\0'; }
    if (buffer[k] == '\0')
    { break; }
  }
  return(buffer);
}

static
void __consume_cursor (lql_cursor_t *cursor)
{
  leela_status rc;
  while ((rc = leela_lql_cursor_next(cursor)) == LEELA_OK)
  {
    switch (leela_lql_fetch_type(cursor))
    {
    case LQL_NAME_MSG:
    {
      lql_name_t *name = leela_lql_fetch_name(cursor);
      fprintf(stdout, "[[\"name\",[\"%s\",\"%s\",\"%s\",\"%s\"]]]\n", name->user, name->tree, name->name, name->guid);
      fflush(stdout);
      leela_lql_name_free(name);
      break;
    }
    case LQL_PATH_MSG:
    {
      lql_path_t *path = leela_lql_fetch_path(cursor);
      fprintf(stdout, "[[\"path\", [");
      for (int k=0; k<path->size; k+=1)
      { fprintf(stdout, "%s[\"%s\",\"%s\"]", (k == 0 ? "" : ","), path->entries[k].fst, path->entries[k].snd); }
      fprintf(stdout, "]]]\n");
      fflush(stdout);
      leela_lql_path_free(path);
      break;
    }
    case LQL_NATTR_MSG:
    {
      lql_nattr_t *nattr = leela_lql_fetch_nattr(cursor);
      fprintf(stdout, "[[\"n-attr\", [\"%s\",[", nattr->guid);
      for (int k=0; k<nattr->size; k+=1)
      {  fprintf(stdout, "%s\"%s\"", (k == 0 ? "" : ","), nattr->names[k]); }
      fprintf(stdout, "]]]]\n");
      fflush(stdout);
      leela_lql_nattr_free(nattr);
      break;
    }
    case LQL_KATTR_MSG:
    {
      lql_kattr_t *kattr = leela_lql_fetch_kattr(cursor);
      fprintf(stdout, "[[\"k-attr\", [\"%s\", \"%s\", ", kattr->guid, kattr->name);
      switch (kattr->value->vtype)
      {
      case LQL_NIL_TYPE:
        fprintf(stdout, "null]]]\n");
        break;
      case LQL_BOOL_TYPE:
        if (kattr->value->data.v_bool)
        { fprintf(stdout, "true]]]\n"); }
        else
        { fprintf(stdout, "false]]]\n"); }
        break;
      case LQL_TEXT_TYPE:
        fprintf(stdout, "\"%s\"]]]\n", kattr->value->data.v_str);
        break;
      case LQL_INT32_TYPE:
        fprintf(stdout, "%ld]]]\n", (long int) kattr->value->data.v_i32);
        break;
      case LQL_INT64_TYPE:
        fprintf(stdout, "%lld]]]\n", (long long int) kattr->value->data.v_i64);
        break;
      case LQL_UINT32_TYPE:
        fprintf(stdout, "%lu]]]\n", (long unsigned int) kattr->value->data.v_u32);
        break;
      case LQL_UINT64_TYPE:
        fprintf(stdout, "%llu]]]\n", (long long unsigned int) kattr->value->data.v_u64);
        break;
      case LQL_DOUBLE_TYPE:
        fprintf(stdout, "%f]]]\n", kattr->value->data.v_double);
        break;
      }
      fflush(stdout);
      leela_lql_kattr_free(kattr);
      break;
    }
    case LQL_FAIL_MSG:
    {
      lql_fail_t *fail = leela_lql_fetch_fail(cursor);
      fprintf(stdout, "[[\"fail\", %d, \"%s\"]]\n", fail->code, fail->message);
      fflush(stdout);
      leela_lql_fail_free(fail);
      break;
    }
    }
  }
  if (rc != LEELA_EOF)
  {
    fprintf(stdout, "[[\"fail\", -1, \"next error\"]]\n");
    fflush(stdout);
  }
}

static
void __mainloop (lql_context_t *ctx, const char *username, const char *secret, int timeout)
{
  char buffer[8192];
  while (strlen(__readline(buffer, 8192)) > 4)
  {
    buffer[strlen(buffer)-2] = '\0';
    const char *query        = __unslash(buffer+2);
    lql_cursor_t *cursor = leela_lql_cursor_init(ctx, username, secret, timeout);
    if (cursor != NULL)
    {
      if (leela_lql_cursor_execute(cursor, query) == LEELA_OK)
      {
        __consume_cursor(cursor);
        fprintf(stdout, "[null]\n");
        fflush(stdout);
      }
      else
      {
        fprintf(stdout, "[[\"fail\", -1, \"execute error\"]]\n");
        fflush(stdout);
      }
      leela_lql_cursor_close(cursor);
    }
    else
    {
      fprintf(stdout, "[[\"fail\", -1, \"init error\"]]\n");
      fflush(stdout);
    }
  }
}

int main (int argc, char *argv[])
{
  char json[8192];
  __readline(json, 8193);
  char *secret                 = __get_string(json, "\"secret\":");
  int timeout                  = 60000;
  char *username               = __get_string(json, "\"username\":");
  leela_endpoint_t **endpoints = __get_endpoint(json, "\"endpoint\":");
  lql_context_t *context       = leela_lql_context_init((const leela_endpoint_t * const *) endpoints);
  if (context != NULL)
  {  __mainloop(context, username, secret, timeout); }
  free(secret);
  free(username);
  leela_lql_context_close(context);
  __free_endpoints(endpoints);

  return(0);
}
