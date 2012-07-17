#!/usr/bin/python
# -*- coding: utf-8; -*-

# Copyright 2012 Juliano Martinez
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
# @author: Juliano Martinez

import os.path
import sys
import logging
from logging import DEBUG
from logging import INFO
from logging import WARNING
from logging import ERROR

def set_level(level):
    logging.getLogger().setLevel(level)

def debug(*args, **kwargs):
    logging.debug(*args, **kwargs)

def info(*args, **kwargs):
    logging.info(*args, **kwargs)

def warn(*args, **kwargs):
    logging.warn(*args, **kwargs)

def error(*args, **kwargs):
    logging.error(*args, **kwargs)

def exception(*args, **kwargs):
    logging.exception(*args, **kwargs)
