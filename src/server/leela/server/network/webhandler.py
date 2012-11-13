#!/usr/bin/python
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
from leela.server import logger
from leela.server.data import pp

class LeelaWebHandler(web.RequestHandler):

    no_keep_alive = True
    no_xsrf       = True

    def initialize(self, **kwargs):
        for (k, v) in kwargs.iteritems():
            setattr(self, k, v)

    def _write_debug(self, chunk):
        if (isinstance(chunk, dict)):
            if (isinstance(chunk.get("debug"), dict)):
                debug = dict(chunk["debug"])
            else:
                debug = {}
            debug["uri"] = self.request.uri
            if (self.get_argument("debug", None) is None):
                if ("debug" in chunk):
                    del(chunk["debug"])
            else:
                chunk["debug"] = debug

    def write(self, chunk):
        self._write_debug(chunk)
        cc  = self.get_argument("callback", "")
        bdy = pp.render_json(chunk)
        if (re.match(r"^[a-zA-Z_][a-zA-Z0-9_]*$", cc)):
            self.set_header("Content-Type", "text/javascript; charset=utf-8")
            bdy = u"%s(%s);" % (cc, bdy)
        else:
            self.set_header("Content-Type", "application/json")
        super(LeelaWebHandler, self).write(bdy)

    def write_error(self, status_code, **kwargs):
        debug  = kwargs.get("debug", {})
        if (not isinstance(debug, dict)):
            debug = {}
        if ("exception" in kwargs):
            e = kwargs["exception"]
            debug["exception"] = str(e)
            if (hasattr(e, "getTraceback")):
                debug["stacktrace"] = e.getTraceback()
            else:
                debug["stacktrace"] = traceback.format_exc()
        rply = { "status": status_code,
                 "reason": httplib.responses[status_code],
                 "debug": debug
               }
        self.write(rply)
        self.finish()

class Always404(LeelaWebHandler):

    def get(self):
        raise(web.HTTPError(404))

    def post(self):
        raise(web.HTTPError(404))

    def head(self):
        raise(web.HTTPError(404))

    def delete(self):
        raise(web.HTTPError(404))

    def options(self):
        raise(web.HTTPError(404))

def logexceptions(f):
    def g (*args, **kwargs):
        try:
            return f(*args, **kwargs)
        except:
            logger.exception()
            raise
    g.__name__ = f.__name__
    return g
