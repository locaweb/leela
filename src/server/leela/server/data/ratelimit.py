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

import time

class RateLimit(object):

    def __init__(self, timeout):
        self.timeout = timeout
        self.memory  = {}

    def should_emit(self, k):
        if (k not in self.memory):
            self.memory[k] = 0
        elapsed = time.time() - self.memory[k]
        if (elapsed > self.timeout):
            self.memory[k] = time.time()
            return(True)
        else:
            return(False)
        
