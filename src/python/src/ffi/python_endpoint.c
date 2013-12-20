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
#include "python_endpoint.h"

PyTypeObject pyleela_endpoint_type = { PyObject_HEAD_INIT(NULL) };

static
PyObject *pyleela_endpoint_alloc(PyTypeObject *, PyObject *, PyObject *);

static
void pyleela_endpoint_free(PyObject *);

static
PyObject *pyleela_endpoint_attr_protocol(PyObject *, void *);

static
PyObject *pyleela_endpoint_attr_path(PyObject *, void *);

static
PyObject *pyleela_endpoint_attr_host(PyObject *, void *);

static
PyObject *pyleela_endpoint_attr_port(PyObject *, void *);

static
PyObject *pyleela_endpoint_repr(PyObject *);

static
PyGetSetDef pyleela_endpoint_accessors[] = {
  {"protocol",
   pyleela_endpoint_attr_protocol, NULL,
   NULL,
   NULL
  },
  {"path",
   pyleela_endpoint_attr_path, NULL,
   NULL,
   NULL
  },
  {"host",
   pyleela_endpoint_attr_host, NULL,
   NULL,
   NULL
  },
  {"port",
   pyleela_endpoint_attr_port, NULL,
   NULL,
   NULL
  },
  {NULL}
};

PyObject *pyleela_endpoint_alloc(PyTypeObject *type, PyObject *args, PyObject *kwargs)
{
  (void) kwargs;
  const char *endpoint;
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) type->tp_alloc(type, 0);
  if (self != NULL)
  {
    self->endpoint = NULL;
    if (! PyArg_ParseTuple(args, "s", &endpoint))
    {
      Py_DECREF(self);
      return(NULL);
    }

    self->endpoint = leela_endpoint_load(endpoint);
    if (self->endpoint == NULL)
    {
      Py_DECREF(self);
      PyErr_SetString(PyExc_RuntimeError, "parse error");
      return(NULL);
    }
  }

  return((PyObject *) self);
}

void pyleela_endpoint_free(PyObject *obj)
{
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) obj;
  leela_endpoint_free(self->endpoint);
  self->ob_type->tp_free((PyObject*) self);
}

PyObject *pyleela_endpoint_attr_protocol(PyObject *self0, void *closure)
{
  (void) closure;
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) self0;
  if (self->endpoint->protocol == PROTO_TCP)
  { return(Py_BuildValue("s", "tcp")); }
  else if (self->endpoint->protocol == PROTO_UDP)
  { return(Py_BuildValue("s", "udp")); }
  else
  { Py_RETURN_NONE; }
}

PyObject *pyleela_endpoint_attr_path(PyObject *self0, void *closure)
{
  (void) closure;
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) self0;
  return(Py_BuildValue("s", self->endpoint->path));
}

PyObject *pyleela_endpoint_attr_host(PyObject *self0, void *closure)
{
  (void) closure;
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) self0;
  return(Py_BuildValue("s", self->endpoint->host));
}

PyObject *pyleela_endpoint_attr_port(PyObject *self0, void *closure)
{
  (void) closure;
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) self0;
  return(Py_BuildValue("i", self->endpoint->port));
}

PyObject *pyleela_endpoint_repr(PyObject *obj)
{
  pyleela_endpoint_t *self = (pyleela_endpoint_t *) obj;
  char *str                = leela_endpoint_dump(self->endpoint);
  if (str == NULL)
  { return(PyErr_NoMemory()); }

  PyObject *repr = PyString_FromString(str);
  free(str);
  return(repr);
}

PyMODINIT_FUNC
init_leela_endpoint(void)
{
  pyleela_endpoint_type.tp_basicsize = sizeof(pyleela_endpoint_t);
  pyleela_endpoint_type.tp_flags     = Py_TPFLAGS_DEFAULT;
  pyleela_endpoint_type.tp_name      = "_leela_endpoint.Endpoint";
  pyleela_endpoint_type.tp_dealloc   = pyleela_endpoint_free;
  pyleela_endpoint_type.tp_getset    = pyleela_endpoint_accessors;
  pyleela_endpoint_type.tp_new       = pyleela_endpoint_alloc;
  pyleela_endpoint_type.tp_repr      = pyleela_endpoint_repr;

  if (PyType_Ready(&pyleela_endpoint_type) != 0)
  { return; }
  Py_INCREF(&pyleela_endpoint_type);

  PyObject *m = Py_InitModule("_leela_endpoint", NULL);
  PyModule_AddObject(m, "Endpoint", (PyObject *) &pyleela_endpoint_type);
}
