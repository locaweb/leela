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
from cyclone import web

class LeelaWebHandler(web.RequestHandler):

    def write(self, chunk):
        cc  = self.get_argument("callback", "")
        if (re.match(r"^[a-zA-Z_][a-zA-Z0-9_]*$", cc)):
            self.set_header("Content-Type", "text/javascript; charset=utf-8")
            bdy = u"%s(%s);" % (cc, json.dumps(chunk))
        else:
            self.set_header("Content-Type", "application/json")
            bdy = json.dumps(chunk)
        super(LeelaHandler, self).write(bdy)

    def write_error(self, status_code, **kwargs):
        debug = {}
        if ("exc_info" in kwargs):
            debug["exception"] = traceback.format_exception(*kwargs["exc_info"])
        rply = { "status": status_code,
                 "reason": httplib.responses[status_code],
                 "debug": debug
               }
        self.write(rply)
        self.finish()
