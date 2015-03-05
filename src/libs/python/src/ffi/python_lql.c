
/* Copyright 2014 (c) Diego Souza                                                  */
/* All rights reserved.                                                            */

/* Redistribution and use in source and binary forms, with or without              */
/* modification, are permitted provided that the following conditions are met:     */

/*   * Redistributions of source code must retain the above copyright notice,      */
/*     this list of conditions and the following disclaimer.                       */
/*   * Redistributions in binary form must reproduce the above copyright notice,   */
/*     this list of conditions and the following disclaimer in the documentation   */
/*     and/or other materials provided with the distribution.                      */
/*   * Neither the name of the <ORGANIZATION> nor the names of its contributors    */
/*     may be used to endorse or promote products derived from this software       */
/*     without specific prior written permission.                                  */

/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND */
/* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   */
/* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          */
/* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE    */
/* FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL      */
/* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      */
/* SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      */
/* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,   */
/* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   */
/* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.            */

#include <Python.h>
#include "python_lql.h"

PyTypeObject pylql_context_type = { PyObject_HEAD_INIT(NULL) };
PyTypeObject pylql_cursor_type  = { PyObject_HEAD_INIT(NULL) };

typedef PyObject*(*to_python_f)(const void *);

typedef struct {
  int     fh;
  char   *buff;
  size_t  size;
} loghandle_t;

static
PyObject *__to_python_from_double(const void *raw)
{ return(PyFloat_FromDouble(*((const double*) raw))); }

static
PyObject *pylql_context_init(PyTypeObject *, PyObject *, PyObject *);

static
PyObject *pylql_context_close(PyObject *, PyObject *);

static
PyObject *pylql_context_cursor(PyObject *, PyObject *);

static
PyObject *pylql_cursor_init(PyTypeObject *, PyObject *, PyObject *);

static
PyObject *pylql_cursor_close(PyObject *, PyObject *);

static
PyObject *pylql_cursor_execute(PyObject *, PyObject *);

static
PyObject *pylql_cursor_next(PyObject *, PyObject *);

static
PyObject *pylql_cursor_fetch(PyObject *, PyObject *);

static
void pylql_context_free(PyObject *);

static
void pylql_cursor_free(PyObject *);

static
void pylql_logwrapper__ (void *ptr, const char *fmt, va_list args)
{
  ssize_t off, tmp, sz;
  loghandle_t *lh = (loghandle_t *) ptr;
  if (lh != NULL)
  {
    if ((sz = vsnprintf(lh->buff, lh->size, fmt, args)) > 0)
    {
      off = 0;
      if (sz + 1 < lh->size)
      { sz += 1; }
      lh->buff[sz-1] = '\n';
      while (off < sz)
      {
        if ((tmp = write(lh->fh, lh->buff + off, sz - off)) <= 0)
        { break; }
        off += tmp;
      }
    }
  }
}

static
PyObject *__make_name_msg(lql_name_t *name)
{
  PyObject *tuple = PyTuple_New(5);
  if (tuple == NULL)
  { return(NULL); }

  int rc = PyTuple_SetItem(tuple, 0, PyString_FromString(name->user))
         | PyTuple_SetItem(tuple, 1, PyString_FromString(name->tree))
         | PyTuple_SetItem(tuple, 2, PyString_FromString(name->kind))
         | PyTuple_SetItem(tuple, 3, PyString_FromString(name->name))
         | PyTuple_SetItem(tuple, 4, PyString_FromString(name->guid));
  if (rc != 0)
  {
    Py_XDECREF(tuple);
    return(NULL);
  }
  return(tuple);
}

static
PyObject *__make_nattr_msg(lql_nattr_t *nattr)
{
  int k, rc;
  PyObject *list  = PyTuple_New(nattr->size);
  PyObject *tuple = PyTuple_New(2);
  if (list == NULL || tuple == NULL)
  {
    Py_XDECREF(list);
    Py_XDECREF(tuple);
    return(NULL);
  }

  rc = PyTuple_SetItem(tuple, 0, PyString_FromString(nattr->guid))
     | PyTuple_SetItem(tuple, 1, list);
  for (k=0; k<nattr->size; k+=1)
  { rc = rc | PyTuple_SetItem(list, k, PyString_FromString(nattr->names[k])); }

  if (rc != 0)
  {
    Py_XDECREF(tuple);
    Py_XDECREF(list);
    return(NULL);
  }
  return(tuple);
}

static
PyObject *__make_pyvalue(const lql_value_t *value)
{
  PyObject *pyvalue = NULL;

  switch (value->vtype)
  {
  case LQL_BOOL_TYPE:
    if (value->data.v_bool)
    { pyvalue = Py_True; }
    else
    { pyvalue = Py_False; }
    Py_INCREF(pyvalue);
    break;
  case LQL_TEXT_TYPE:
    pyvalue = PyString_FromString(value->data.v_str);
    break;
  case LQL_INT32_TYPE:
    pyvalue = PyInt_FromLong(value->data.v_i32);
    break;
  case LQL_UINT32_TYPE:
    pyvalue = PyInt_FromLong(value->data.v_u32);
    break;
  case LQL_INT64_TYPE:
    pyvalue = PyLong_FromLongLong(value->data.v_i64);
    break;
  case LQL_UINT64_TYPE:
    pyvalue = PyLong_FromLongLong(value->data.v_u64);
    break;
  case LQL_DOUBLE_TYPE:
    pyvalue = PyFloat_FromDouble(value->data.v_double);
    break;
  case LQL_NIL_TYPE:
    Py_INCREF(Py_None);
    pyvalue = Py_None;
    break;
  };

  return(pyvalue);
}

static
PyObject *__make_list_of_tuples(int size, lql_tuple2_t *entries, to_python_f parse_fst, to_python_f parse_snd)
{
  int k;
  PyObject *entry;
  PyObject *tuple = PyTuple_New(size);
  if (tuple == NULL)
  { return(NULL); }

  for (k=0; k<size; k+=1)
  {
    entry = PyTuple_New(2);
    if (entry == NULL)
    {
      Py_XDECREF(tuple);
      return(NULL);
    }

    if (PyTuple_SetItem(entry, 0, parse_fst(entries[k].fst)) != 0
        || PyTuple_SetItem(entry, 1, parse_snd(entries[k].snd)) != 0)
    {
      Py_XDECREF(tuple);
      Py_XDECREF(entry);
      return(NULL);
    }

    if (PyTuple_SetItem(tuple, k, entry) != 0)
    {
      Py_XDECREF(tuple);
      return(NULL);
    }
  }

  return(tuple);
}

static
PyObject *__make_tattr_msg(lql_tattr_t *tattr)
{
  PyObject *tuple = PyTuple_New(3);
  if (tuple == NULL)
  { return(NULL); }

  int rc = PyTuple_SetItem(tuple, 0, PyString_FromString(tattr->guid))
    | PyTuple_SetItem(tuple, 1, PyString_FromString(tattr->name))
    | PyTuple_SetItem(tuple, 2, __make_list_of_tuples(tattr->size, tattr->series, __to_python_from_double, (to_python_f) __make_pyvalue));

  if (rc != 0)
  {
    Py_XDECREF(tuple);
    return(NULL);
  }

  return(tuple);
}

static
PyObject *__make_kattr_msg(lql_kattr_t *kattr)
{
  PyObject *tuple = PyTuple_New(3);
  if (tuple == NULL)
  { return(NULL); }

  int rc = PyTuple_SetItem(tuple, 0, PyString_FromString(kattr->guid))
         | PyTuple_SetItem(tuple, 1, PyString_FromString(kattr->name))
         | PyTuple_SetItem(tuple, 2, __make_pyvalue(kattr->value));

  if (rc != 0)
  {
    Py_XDECREF(tuple);
    return(NULL);
  }

  return(tuple);
}

static
PyObject *__make_path_msg(lql_path_t *path)
{ return(__make_list_of_tuples(path->size, path->entries, (to_python_f) PyString_FromString, (to_python_f) PyString_FromString)); }

static
PyObject *__make_stat_msg(lql_stat_t *stat)
{ return(__make_list_of_tuples(stat->size, stat->attrs, (to_python_f) PyString_FromString, (to_python_f) PyString_FromString)); }

static
void __make_fail_msg(lql_fail_t *fail)
{
  PyObject *pModule = PyImport_ImportModule("pyleela.exception");
  PyObject *pClass = NULL;
  PyObject *pTuple = NULL;

  if (pModule != NULL && fail != NULL)
  {
    if (fail->code == 400)
    {
      pClass = PyObject_GetAttrString(pModule, "BadRequestError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
    else if (fail->code == 403)
    {
      pClass = PyObject_GetAttrString(pModule, "ForbiddenError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
    else if (fail->code == 404)
    {
      pClass = PyObject_GetAttrString(pModule, "NotFoundError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
    else if (fail->code == 500)
    {
      pClass = PyObject_GetAttrString(pModule, "InternalServerError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
    else if (fail->code > 400 && fail->code < 500)
    {
      pClass = PyObject_GetAttrString(pModule, "UserError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
    else if (fail->code > 500 && fail->code < 600)
    {
      pClass = PyObject_GetAttrString(pModule, "ServerError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
    else
    {
      pClass = PyObject_GetAttrString(pModule, "LeelaError");
      pTuple = Py_BuildValue("si", fail->message, fail->code);
      PyErr_SetObject(pClass, pTuple);
    }
  }
  else
  { PyErr_SetString(PyExc_RuntimeError, "unexpected error"); }

  Py_XDECREF(pModule);
  Py_XDECREF(pClass);
  Py_XDECREF(pTuple);
}

static
void __throw_exception(pylql_cursor_t *cursor)
{
  lql_fail_t *fail;
  Py_BEGIN_ALLOW_THREADS
  fail = leela_lql_fetch_fail(cursor->cursor);
  Py_END_ALLOW_THREADS
  __make_fail_msg(fail);
  leela_lql_fail_free(fail);
}

static
PyMethodDef pylql_context_methods[] = {
  {"close", pylql_context_close, METH_VARARGS,
   NULL
  },
  {"cursor", pylql_context_cursor, METH_VARARGS,
   NULL
  },
  {NULL}
};

static
PyMethodDef pylql_cursor_methods[] = {
  {"close", pylql_cursor_close, METH_VARARGS,
   NULL
  },
  {"execute", pylql_cursor_execute, METH_VARARGS,
   NULL,
  },
  {"next", pylql_cursor_next, METH_VARARGS,
   NULL
  },
  {"fetch", pylql_cursor_fetch, METH_VARARGS,
   NULL,
  },
  /*
  {"restart", pylql_cursor_restart, METH_VARARGS,
   NULL,
  },
  */
  {NULL}
};

PyMODINIT_FUNC
init_leela_lql(void)
{
  pylql_context_type.tp_basicsize = sizeof(pylql_context_t);
  pylql_context_type.tp_flags     = Py_TPFLAGS_DEFAULT;
  pylql_context_type.tp_name      = "_leela_lql.Context";
  pylql_context_type.tp_methods   = pylql_context_methods;
  pylql_context_type.tp_dealloc   = pylql_context_free;
  pylql_context_type.tp_new       = pylql_context_init;

  pylql_cursor_type.tp_basicsize = sizeof(pylql_cursor_t);
  pylql_cursor_type.tp_flags     = Py_TPFLAGS_DEFAULT;
  pylql_cursor_type.tp_name      = "_leela_lql.Cursor";
  pylql_cursor_type.tp_methods   = pylql_cursor_methods;
  pylql_cursor_type.tp_dealloc   = pylql_cursor_free;
  pylql_cursor_type.tp_new       = pylql_cursor_init;

  if (PyType_Ready(&pylql_context_type) != 0)
  { return; }
  Py_INCREF(&pylql_context_type);

  if (PyType_Ready(&pylql_cursor_type) != 0)
  { return; }
  Py_INCREF(&pylql_cursor_type);

  PyObject *m = Py_InitModule("_leela_lql", NULL);
  PyModule_AddObject(m, "Context", (PyObject *) &pylql_context_type);
  PyModule_AddObject(m, "Cursor", (PyObject *) &pylql_cursor_type);
}

PyObject *pylql_context_init(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
  (void) kwargs;
  PyObject *pyendpoints;
  int timeout                   = 0;
  char *secret                  = NULL;
  char *username                = NULL;
  size_t k;
  PyObject *item;
  loghandle_t *debug_lh         = (loghandle_t *) malloc(sizeof(loghandle_t));
  loghandle_t *trace_lh         = (loghandle_t *) malloc(sizeof(loghandle_t));
  pylql_context_t *self         = NULL;
  leela_endpoint_t **cendpoints = NULL;

  self = (pylql_context_t *) type->tp_alloc(type, 0);
  if (self != NULL)
  {
    self->context = NULL;

    if (debug_lh == NULL || trace_lh == NULL)
    { goto handle_error; }
    
    debug_lh->fh   = -1;
    debug_lh->size = 4096;
    debug_lh->buff = (char *) malloc(sizeof(char) * debug_lh->size);

    trace_lh->fh   = -1;
    trace_lh->size = 4096;
    trace_lh->buff = (char *) malloc(sizeof(char) * debug_lh->size);
  
    if (! PyArg_ParseTuple(args, "O!ssi|ii", &PyList_Type, &pyendpoints, &username, &secret, &timeout, &debug_lh->fh, &trace_lh->fh))
    { goto handle_error; }
    
    cendpoints = (leela_endpoint_t **) malloc((PyList_Size(pyendpoints) + 1) * sizeof(leela_endpoint_t *));
    if (cendpoints == NULL)
    {
      PyErr_SetString(PyExc_MemoryError, "can't allocate endpoint array");
      goto handle_error;
    }

    for (k=0; k<PyList_Size(pyendpoints); k+=1)
    {
      item           = PyList_GetItem(pyendpoints, k);
      cendpoints[k]  = NULL;
      if (PyString_Check(item) == 0)
      {
        PyErr_SetString(PyExc_TypeError, "list members must be str");
        goto handle_error;
      }
      cendpoints[k] = leela_endpoint_load(PyString_AsString(item));
      if (cendpoints[k] == NULL)
      {
        PyErr_SetString(PyExc_RuntimeError, "bad endpoint");
        goto handle_error;
      }
    }
    cendpoints[PyList_Size(pyendpoints)] = NULL;

    Py_BEGIN_ALLOW_THREADS
    self->context = leela_lql_context_init2((const leela_endpoint_t * const *) cendpoints, username, secret, timeout, pylql_logwrapper__, debug_lh, pylql_logwrapper__, trace_lh);
    Py_END_ALLOW_THREADS
    if (self->context == NULL)
    {
      PyErr_SetString(PyExc_RuntimeError, "error initializing the context!");
      goto handle_error;
    }
  }

  for (k=0; cendpoints != NULL && cendpoints[k] != NULL; k+=1)
  { leela_endpoint_free(cendpoints[k]); }
  free(cendpoints);

  return((PyObject *) self);

handle_error:
  for (k=0; cendpoints != NULL && cendpoints[k] != NULL; k+=1)
  { leela_endpoint_free(cendpoints[k]); }
  free(debug_lh);
  free(trace_lh);
  free(cendpoints);
  Py_XDECREF(self);
  return(NULL);
}

PyObject *pylql_cursor_init(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
  (void) kwargs;
  int timeout          = 0;
  const char *secret   = NULL;
  const char *username = NULL;
  const char *endpoint = NULL;
  leela_endpoint_t *e  = NULL;
  pylql_context_t *context;

  pylql_cursor_t *self = (pylql_cursor_t *) type->tp_alloc(type, 0);
  if (self != NULL)
  {
    self->cursor = NULL;
    if (! PyArg_ParseTuple(args, "O!|ssis", &pylql_context_type, &context, &username, &secret, &timeout, &endpoint))
    {
      Py_XDECREF(self);
      return(NULL);
    }

    if (endpoint != NULL)
    { e = leela_endpoint_load(endpoint); }

    Py_BEGIN_ALLOW_THREADS
    if (e != NULL)
    { self->cursor = leela_lql_cursor_init_on(context->context, e, username, secret, timeout); }
    else
    { self->cursor = leela_lql_cursor_init(context->context, username, secret, timeout); }
    Py_END_ALLOW_THREADS

    leela_endpoint_free(e);
    if (self->cursor == NULL)
    {
      Py_XDECREF(self);
      PyErr_SetString(PyExc_RuntimeError, "parse error");
      return(NULL);
    }
  }

  return((PyObject *) self);
}

PyObject *pylql_context_close(PyObject *self, PyObject *args)
{
  (void) args;
  pylql_context_t *context = (pylql_context_t *) self;
  Py_BEGIN_ALLOW_THREADS
  leela_lql_context_close(context->context);
  Py_END_ALLOW_THREADS
  context->context = NULL;
  Py_RETURN_NONE;
}

PyObject *pylql_cursor_close(PyObject *self, PyObject *args)
{
  (void) args;
  pylql_cursor_t *cursor = (pylql_cursor_t *) self;
  Py_BEGIN_ALLOW_THREADS
  leela_lql_cursor_close(cursor->cursor);
  Py_END_ALLOW_THREADS
  cursor->cursor = NULL;
  Py_RETURN_NONE;
}

/* TO BE IMPLEMENTED
PyObject *pylql_cursor_restart(PyObject *self, PyObject *args)
{
  (void) args;
  pylql_cursor_t *cursor = (pylql_cursor_t *) self;
  if (leela_lql_cursor_restart(cursor->cursor) != 0)
  { PyErr_SetString(PyExc_RuntimeError, "error restarting cursor"); }
  Py_RETURN_NONE;
}
*/

void pylql_cursor_free(PyObject *self)
{ self->ob_type->tp_free(self); }

PyObject *pylql_context_cursor(PyObject *self, PyObject *args)
{
  int timeout          = 0;
  const char *secret   = NULL;
  const char *username = NULL;
  PyObject *myargs;

  if (!PyArg_ParseTuple(args, "|ssi", &username, &secret, &timeout))
  { return(NULL); }

  if (timeout > 0)
  { myargs = Py_BuildValue("Ossi", self, username, secret, timeout); }
  else if (secret != NULL)
  { myargs = Py_BuildValue("Oss", self, username, secret); }
  else if (username != NULL)
  { myargs = Py_BuildValue("Os", self, username); }
  else
  { myargs = Py_BuildValue("(O)", self); }

  if (myargs == NULL)
  { return(NULL); }

  PyObject *result = pylql_cursor_init(&pylql_cursor_type, myargs, NULL);
  Py_XDECREF(myargs);
  return(result);
}

PyObject *pylql_cursor_execute(PyObject *self, PyObject *args)
{
  const char *query;
  pylql_cursor_t *cursor = (pylql_cursor_t *) self;

  if (! PyArg_ParseTuple(args, "s", &query))
  { return(NULL); }

  leela_status rc;
  Py_BEGIN_ALLOW_THREADS
  rc = leela_lql_cursor_execute(cursor->cursor, query);
  Py_END_ALLOW_THREADS
  if (rc == LEELA_OK)
  { Py_RETURN_NONE; }

  PyErr_SetString(PyExc_RuntimeError, "could not execute query!");
  return(NULL);
}

PyObject *pylql_cursor_next(PyObject *self, PyObject *args)
{
  pylql_cursor_t *cursor = (pylql_cursor_t *) self;

  leela_status rc;

  Py_BEGIN_ALLOW_THREADS
  rc = leela_lql_cursor_next(cursor->cursor);
  Py_END_ALLOW_THREADS
  if (rc == LEELA_OK)
  { Py_RETURN_TRUE; }
  else if (rc == LEELA_EOF)
  { Py_RETURN_FALSE; }
  else
  {
    PyErr_SetString(PyExc_RuntimeError, "error reading");
    return(NULL);
  }
}

PyObject *pylql_cursor_fetch(PyObject *self, PyObject *args)
{
  pylql_cursor_t *cursor = (pylql_cursor_t *) self;
  lql_row_type row       = leela_lql_fetch_type(cursor->cursor);
  PyObject *pyrow        = PyTuple_New(2);
  PyObject *value        = NULL;
  PyObject *type         = NULL;
  if (pyrow == NULL)
  { return(NULL); }

  if (row == LQL_NAME_MSG)
  {
    lql_name_t *name;
    Py_BEGIN_ALLOW_THREADS
    name = leela_lql_fetch_name(cursor->cursor);
    Py_END_ALLOW_THREADS
    if (name != NULL)
    {
      value = __make_name_msg(name);
      type  = PyString_FromString("name");
      leela_lql_name_free(name);
    }
  }
  else if (row == LQL_PATH_MSG)
  {
    lql_path_t *path;
    Py_BEGIN_ALLOW_THREADS
    path = leela_lql_fetch_path(cursor->cursor);
    Py_END_ALLOW_THREADS
    if (path != NULL)
    {
      value = __make_path_msg(path);
      type  = PyString_FromString("path");
      leela_lql_path_free(path);
    }
  }
  else if (row == LQL_STAT_MSG)
  {
    lql_stat_t *stat;
    Py_BEGIN_ALLOW_THREADS
    stat = leela_lql_fetch_stat(cursor->cursor);
    Py_END_ALLOW_THREADS
    if (stat != NULL)
    {
      value = __make_stat_msg(stat);
      type  = PyString_FromString("stat");
      leela_lql_stat_free(stat);
    }
  }
  else if (row == LQL_NATTR_MSG)
  {
    lql_nattr_t *nattr;
    Py_BEGIN_ALLOW_THREADS
    nattr = leela_lql_fetch_nattr(cursor->cursor);
    Py_END_ALLOW_THREADS
    if (nattr != NULL)
    {
      value = __make_nattr_msg(nattr);
      type  = PyString_FromString("n-attr");
      leela_lql_nattr_free(nattr);
    }
  }
  else if (row == LQL_KATTR_MSG)
  {
    lql_kattr_t *kattr;
    Py_BEGIN_ALLOW_THREADS
    kattr = leela_lql_fetch_kattr(cursor->cursor);
    Py_END_ALLOW_THREADS
    if (kattr != NULL)
    {
      value = __make_kattr_msg(kattr);
      type  = PyString_FromString("k-attr");
      leela_lql_kattr_free(kattr);
    }
  }
  else if (row == LQL_TATTR_MSG)
  {
    lql_tattr_t *tattr;
    Py_BEGIN_ALLOW_THREADS
    tattr = leela_lql_fetch_tattr(cursor->cursor);
    Py_END_ALLOW_THREADS
    if (tattr != NULL)
    {
      value = __make_tattr_msg(tattr);
      type  = PyString_FromString("t-attr");
      leela_lql_tattr_free(tattr);
    }
  }
  else if (row == LQL_FAIL_MSG)
  { __throw_exception(cursor); }

  if (type == NULL || value == NULL)
  {
    Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(pyrow);
    if (PyErr_Occurred() == NULL)
    { PyErr_SetString(PyExc_RuntimeError, "error reading"); }
    return(NULL);
  }

  if (PyTuple_SetItem(pyrow, 0, type) != 0
      || PyTuple_SetItem(pyrow, 1, value) != 0)
  {
    Py_XDECREF(pyrow);
    return(NULL);
  }

  return(pyrow);
}

void pylql_context_free(PyObject *self)
{ self->ob_type->tp_free(self); }
