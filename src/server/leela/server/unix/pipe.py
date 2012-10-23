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
import posix
from twisted.python import failure
from twisted.internet import error
from twisted.internet import fdesc
from twisted.internet.main import CONNECTION_LOST
from twisted.internet.main import CONNECTION_DONE
from twisted.internet.abstract import FileDescriptor
from leela.server import funcs
from leela.server import logger

class UnixPipe(FileDescriptor):

    def __init__(self, name, mode, reactor=None):
        FileDescriptor.__init__(self, reactor=reactor)
        self._pipebuf = max(posix.pathconf(name, "PC_PIPE_BUF"), 4096)
        self._wbuff   = []
        self._md      = mode
        self._fn      = name
        self._fd      = None
        self._ar      = True
        self._maxq    = 1000000

    def autoretry(self, v):
        self._ar = v

    def _doconnect(self):
        if (self._fd is None):
            if (self._md == "r"):
                self._fd = os.open(self._fn, os.O_RDONLY|os.O_NONBLOCK)
                fdesc._setCloseOnExec(self._fd)
                self.startReading()
            elif (self._md == "w"):
                # N.B.: Posix leaves this undefined, i.e., opening a
                #       fifo with RDWR. However, Linux allows one
                #       opening a fifo even without a reader
                #       available.
                self._fd = os.open(self._fn, os.O_RDWR|os.O_NONBLOCK)
                fdesc._setCloseOnExec(self._fd)
            else:
                raise(RuntimeError())

    def connect(self):
        logger.warn("connecting to unix pipe [mode: %s, pipebuf: %d]: %s" % (self._md, self._pipebuf, self._fn))
        self._doconnect()
        self.connected    = True
        self.disconnected = False

    def disconnect(self):
        self.loseConnection()

    def connectionLost(self, reason):
        logger.warn("lost connection with pipe: %s/%s" % (self._fn, reason.getErrorMessage()))
        FileDescriptor.connectionLost(self, reason)
        funcs.suppress(os.close)(self._fd)
        self._fd = None
        if (self._ar):
            self.reactor.callLater(1, self.connect)

    def fileno(self):
        return(self._fd)

    def _flushqueue(self):
        while (len(self._wbuff) > 0):
            c = self._wbuff[0]
            r = self._dowrite_chunk(c)
            if (r is not None):
                return(r)
            self._wbuff.pop(0)

    def _dowrite_chunk(self, chunk):
        # this method assumes 1) fileno is a posix fifo and 2) chunk
        # <= PIPE_BUF. Failing to meet these will make things break
        # bad.
        try:
            os.write(self.fileno(), chunk)
        except (OSError, IOError), io:
            if io.errno in (errno.EAGAIN, errno.EINTR):
                return(0)
            return(CONNECTION_LOST)

    def write(self, chunk):
        return(self.write_chunks([chunk]))

    def write_chunks(self, seq):
        if (not self.connected):
            return
        if (len(self._wbuff) >= self._maxq):
            logger.error("maximum queue size [%d] reached, dropping chunk" % self._maxq)
            return
        total  = 0
        chunks = [buffer("")]
        for chunk in map(buffer, seq):
            size = len(chunk)
            if (size > self._pipebuf):
                raise(RuntimeError("chunks > %d is not supported [nonatomic]" % self._pipebuf))
            if (total+size > self._pipebuf):
                total = size
                chunks.append(chunk)
            else:
                total     += size
                chunks[-1] = buffer(chunks[-1] + chunk)
        self._wbuff.extend(chunks)
        self.startWriting()

    def doWrite(self):
        r = self._flushqueue()
        if (len(self._wbuff) == 0):
            self.stopWriting()
        return(r)

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
