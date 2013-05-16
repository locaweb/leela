# -*- coding: utf-8; -*-
#
# All Rights Reserved.
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
#

import re
import httplib
import traceback
import time
from cyclone import web
from twisted.python.failure import Failure
from telephus.cassandra.c08.ttypes import InvalidRequestException
from leela.server import logger
from leela.server.data import pp
from leela.server.data import excepts

class RestHandler(web.RequestHandler):

    no_keep_alive = True
    no_xsrf       = True

    def initialize(self, **kwargs):
        for (k, v) in kwargs.iteritems():
            setattr(self, k, v)

    def compute_etag(self):
        return(None)

    def _write_debug(self, chunk):
        if (isinstance(chunk, dict)):
            if (isinstance(chunk.get("debug"), dict)):
                debug = dict(chunk["debug"])
            else:
                debug = {}
            debug["uri"] = self.request.uri
            debug["now"] = time.strftime("%Y%m%dT%H%M%S%z")
            if (self.get_argument("debug", None) is None):
                if ("debug" in chunk):
                    del(chunk["debug"])
            else:
                chunk["debug"] = debug

    def write(self, chunk, cache=300):
        self._write_debug(chunk)
        cc  = self.get_argument("callback", "")
        bdy = pp.render_json(chunk)
        self.set_header("Cache-Control", "public, max-age=%d" % cache)
        self.set_header("Access-Control-Allow-Origin", "*")
        if (re.match(r"^[a-zA-Z_][a-zA-Z0-9_]*$", cc)):
            self.set_status(200)
            self.set_header("Content-Type", "text/javascript; charset=utf-8")
            bdy = u"%s(%s);" % (cc, bdy)
        else:
            self.set_header("Content-Type", "application/json")
        super(RestHandler, self).write(bdy)

    def catch(self, exception):
        if (hasattr(exception, "value")):
            e = exception2http(exception.value)
        else:
            e = exception2http(exception)
        return(self.write_error(e.status_code, exception=e))

    def write_error(self, status_code, **kwargs):
        debug  = kwargs.get("debug", {})
        if (not isinstance(debug, dict)):
            debug = {}
        if ("exception" in kwargs):
            e = kwargs["exception"]
            if (not isinstance(e, web.HTTPError)):
                e = exception2http(e)
            if (len(e.args) == 1 \
                and isinstance(e.args[0], dict) \
                and "stacktrace" in e.args[0]):
                debug["stacktrace"] = e.args[0]["stacktrace"]
            debug["exception"]  = str(e)
        self.set_status(status_code)
        rply = { "status": status_code,
                 "reason": httplib.responses[status_code],
                 "debug": debug
               }
        self.write(rply, 0)
        self.finish()

class Always404(RestHandler):

    def get(self):
        raise(web.HTTPError(404))

    def post(self):
        raise(web.HTTPError(404))

    def head(self):
        raise(web.HTTPError(404))

    def delete(self):
        raise(web.HTTPError(404))

def exception2http(e):
    if (hasattr(e, "getTraceback")):
        stacktrace = e.getTraceback()
    else:
        stacktrace = traceback.format_exc()
    if (isinstance(e, web.HTTPError)):
        return(e)
    elif (isinstance(e, KeyError) or isinstance(e, ValueError)):
        return(web.HTTPError(400, None, {"stacktrace": stacktrace}))
    elif (isinstance(e, excepts.NotFoundExcept) or isinstance(e, InvalidRequestException)):
        # XXX: big assumption: InvalidRequestException means we
        #      couldn't find the table.
        return(web.HTTPError(404))
    else:
        return(web.HTTPError(500, None, {"stacktrace": stacktrace}))

def catch(f):
    def g (*args, **kwargs):
        try:
            return f(*args, **kwargs)
        except Exception, e:
            raise(exception2http(e))
    g.__name__ = f.__name__
    return g
