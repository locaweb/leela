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
import json
import httplib
import traceback
import time
from twisted.python.failure import Failure
from cyclone import web
from leela.server import logger

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
            debug["request_uri"]  = self.request.uri
            debug["request_time"] = self.walltime()
            if (self.get_argument("debug", None) is None):
                if ("debug" in chunk):
                    del(chunk["debug"])
            else:
                chunk["debug"] = debug

    def write(self, chunk):
        self._write_debug(chunk)
        cc  = self.get_argument("callback", "")
        bdy = json.dumps(chunk, allow_nan=True, sort_keys=True)
        if (re.match(r"^[a-zA-Z_][a-zA-Z0-9_]*$", cc)):
            self.set_header("Content-Type", "text/javascript; charset=utf-8")
            bdy = u"%s(%s);" % (cc, bdy)
        else:
            self.set_header("Content-Type", "application/json")
        super(LeelaWebHandler, self).write(bdy)

    def write_error(self, status_code, **kwargs):
        debug = {}
        if ("exception" in kwargs):
            e = kwargs["exception"]
            if (isinstance(e, Failure)):
                debug["exception"] = e.getTraceback()
            elif (isinstance(e, Exception)):
                if (not isinstance(e, web.HTTPError)):
                    debug["exception"] = traceback.format_exc()
            else:
                debug["exception"] = str(e)
        rply = { "status": status_code,
                 "reason": httplib.responses[status_code],
                 "debug": debug
               }
        self.write(rply)
        self.finish()

    def walltime(self):
        past_walltime  = self._walltime
        self._walltime = time.time()
        return(self._walltime - past_walltime)

    def prepare(self):
        self._walltime = time.time()

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
