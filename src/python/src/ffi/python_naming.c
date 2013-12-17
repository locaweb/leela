/* Copyright (c) 2011, Diego Souza                                                 */
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
#include <leela/naming.h>
#include <leela/endpoint.h>
#include "python_naming.h"
#include "python_endpoint.h"

static
PyObject *pyleela_naming_start(PyObject *, PyObject *);

static
PyObject *pyleela_naming_stop(PyObject *, PyObject *);

static
PyObject *pyleela_naming_query(PyObject *, PyObject *);

static 
PyMethodDef __functions[] = {
  {"start",
   pyleela_naming_start,
   METH_VARARGS,
   NULL
  },
  {"stop",
   pyleela_naming_stop,
   METH_VARARGS,
   NULL
  },
  {"query",
   pyleela_naming_query,
   METH_VARARGS,
   NULL
  },
  {NULL, NULL, 0, NULL}
};

static
PyObject *__pyendpoint_type()
{
  PyObject *pyendpoint = PyImport_ImportModule("_leela_endpoint");
  if (pyendpoint == NULL)
  { return(NULL); }

  PyObject *pyendpoint_type = PyObject_GetAttrString(pyendpoint, "Endpoint");
  Py_DECREF(pyendpoint);
  return(pyendpoint_type);
}

PyMODINIT_FUNC
init_leela_naming(void)
{ Py_InitModule("_leela_naming", __functions); }

PyObject *pyleela_naming_start(PyObject *self, PyObject *args)
{
  (void) self;
  int timeout;
  PyObject *pytype;
  const char *path;
  pyleela_endpoint_t *endpoint;

  pytype = __pyendpoint_type();
  if (pytype == NULL)
  { return(NULL); }

  if (! PyArg_ParseTuple(args, "O!si", pytype, &endpoint, &path, &timeout))
  {
    Py_DECREF(pytype);
    return(NULL);
  }
  Py_DECREF(pytype);

  leela_naming_t *naming = leela_naming_start(endpoint->endpoint, path, timeout);
  if (naming == NULL)
  { Py_RETURN_NONE; }

#if PY_VERSION_HEX < 0x02070000
  return(PyCObject_FromVoidPtr(naming, NULL));
#else
  return(PyCapsule_New(naming, "_leela_naming.naming", NULL));
#endif
}

#if PY_VERSION_HEX < 0x02070000
static
PyObject *pyleela_naming_stop(PyObject *self, PyObject *args)
{
  (void) self;
  PyObject *capsule;

  if (! PyArg_ParseTuple(args, "O!", &PyCObject_Type, &capsule))
  { return(NULL); }

  leela_naming_t *naming = (leela_naming_t *) PyCObject_AsVoidPtr(capsule);
  if (naming != NULL)
  { leela_naming_stop(naming); }

  Py_RETURN_NONE;
}
#else
static
PyObject *pyleela_naming_stop(PyObject *self, PyObject *args)
{
  (void) self;
  PyObject *capsule;

  if (! PyArg_ParseTuple(args, "O!", &PyCapsule_Type, &capsule))
  { return(NULL); }

  leela_naming_t *naming = (leela_naming_t *) PyCapsule_GetPointer(capsule, "_leela_naming.naming");
  if (naming != NULL)
  { leela_naming_stop(naming); }

  Py_RETURN_NONE;
}
#endif

PyObject *pyleela_naming_query(PyObject *self, PyObject *args)
{
  (void) self;
  PyObject *capsule;
  PyObject *list                 = NULL;
  PyObject *pytype               = __pyendpoint_type();
  leela_naming_value_t *snapshot = NULL;
  leela_naming_value_t *iterator;
  if (pytype == NULL)
  { return(NULL); }

#if PY_VERSION_HEX < 0x02070000
  if (! PyArg_ParseTuple(args, "O!", &PyCObject_Type, &capsule))
  { return(NULL); }

  leela_naming_t *naming = (leela_naming_t *) PyCObject_AsVoidPtr(capsule);
  if (naming == NULL)
  { goto handle_error; }
#else
  if (! PyArg_ParseTuple(args, "O!", &PyCapsule_Type, &capsule))
  { return(NULL); }

  leela_naming_t *naming = (leela_naming_t *) PyCapsule_GetPointer(capsule, "_leela_naming.naming");
  if (naming == NULL)
  { goto handle_error; }
#endif

  snapshot = leela_naming_query(naming);
  iterator = snapshot;
  size_t count;
  for (count=0; iterator != NULL; count+=1)
  { iterator = iterator->next; }

  list     = PyList_New(count);
  iterator = snapshot;
  for (count=0; iterator != NULL; count+=1)
  {
    pyleela_endpoint_t *endpoint = (pyleela_endpoint_t *) PyObject_New(pyleela_endpoint_t, (PyTypeObject *) pytype);
    if (endpoint != NULL)
    { endpoint->endpoint = leela_endpoint_dup(iterator->endpoint); }
    if (endpoint == NULL
        || endpoint->endpoint == NULL
        || PyList_SetItem(list, count, (PyObject *) endpoint) != 0)
    { goto handle_error; }
    iterator = iterator->next;
  }
  Py_DECREF(pytype);
  leela_naming_value_free(snapshot);
  return(list);

handle_error:
  leela_naming_value_free(snapshot);
  Py_XDECREF(list);
  Py_DECREF(pytype);
  return(NULL);
}
