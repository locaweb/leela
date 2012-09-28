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

import os
import errno
from twisted.python import log
from twisted.internet import error
from twisted.internet import fdesc
from twisted.internet.main import CONNECTION_LOST
from twisted.internet.main import CONNECTION_DONE
from twisted.internet.abstract import FileDescriptor
from leela.server import funcs
from leela.server import logger

class UnixPipeReader(FileDescriptor):

    def __init__(self, name, reactor=None):
        FileDescriptor.__init__(self, reactor=reactor)
        self._fn = name
        self._fd = None
        self._ar = True

    def autoretry(self, v):
        self._ar = v

    def connect(self):
        logger.debug("connecting to unix pipe: "+ self._fn)
        if (self._fd is None):
            self._fd = os.open(self._fn, os.O_RDONLY|os.O_NONBLOCK)
            fdesc.setNonBlocking(self._fd)
            fdesc._setCloseOnExec(self._fd)
            self.connected    = True
            self.disconnected = False
            self.startReading()

    def disconnect(self):
        logger.debug("disconnecting from unix pipe: "+ self._fn)
        self.loseConnection()

    def connectionLost(self, reason):
        FileDescriptor.connectionLost(self, reason)
        funcs.suppress(os.close)(self._fd)
        self._fd = None
        if (self._ar):
            self.reactor.callLater(1, self.connect)

    def fileno(self):
        return(self._fd)

    def doRead(self):
        try:
            output = os.read(self._fd, 65536)
        except (OSError, IOError), ioe:
            if ioe.args[0] in (errno.EAGAIN, errno.EINTR):
                return
            else:
                return CONNECTION_LOST
        # if (output == ""):
        #     return(CONNECTION_DONE)
        self.recv_data(output)

    def recv_data(self, data):
        pass
