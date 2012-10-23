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

class MAvg(object):

    def __init__(self, samples):
        self.memory  = {}
        self.samples = samples

    def compute(self, k, e):
        vs = None
        if (k not in self.memory):
            vs = [(e.unixtimestamp(), e.value())]
            self.memory[k] = vs
        else:
            vs = self._insert(k, e)
        return(sum([pair[1] for pair in vs]) / float(len(vs)))

    def _insert(self, k, e):
        vs = self.memory[k]
        if (e.unixtimestamp() > vs[-1][0]):
            l = len(vs)
            for k in range(l):
                if (e.unixtimestamp() > vs[k][0]):
                    vs.insert(k, (e.unixtimestamp(), e.value()))
                    break
            del(vs[self.samples:])
        return(vs)

        
