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

    def __init__(self, name, mode, reactor=None):
        FileDescriptor.__init__(self, reactor=reactor)
        self._md = mode
        self._fn = name
        self._fd = None
        self._ar = True

    def autoretry(self, v):
        self._ar = v

    def connect(self):
        logger.info("connecting to unix pipe: "+ self._fn)
        if (self._fd is None):
            if (self._md == "r"):
                self._fd = os.open(self._fn, os.O_RDONLY|os.O_NONBLOCK)
                fdesc._setCloseOnExec(self._fd)
            elif (self._md == "w"):
                self._fd = os.open(self._fn, os.O_WRONLY)
                fdesc.setNonBlocking(self._fd)
                fdesc._setCloseOnExec(self._fd)
            else:
                raise(RuntimeError())
            self.connected    = True
            self.disconnected = False
            if ("r" == self._md):
                self.startReading()

    def disconnect(self):
        self.loseConnection()

    def connectionLost(self, reason):
        logger.info("lost connection with pipe")
        FileDescriptor.connectionLost(self, reason)
        funcs.suppress(os.close)(self._fd)
        self._fd = None
        if (self._ar):
            self.reactor.callLater(1, self.connect)

    def fileno(self):
        return(self._fd)

    def writeSomeData(self, data):
        return(fdesc.writeToFD(self._fd, data))

    def doRead(self):
        try:
            output = os.read(self._fd, 65536)
        except (OSError, IOError), ioe:
            if ioe.args[0] in (errno.EAGAIN, errno.EINTR):
                return 0
            else:
                return CONNECTION_LOST
        if not output:
            return CONNECTION_DONE
        self.recv_data(output)

    def recv_data(self, data):
        pass
