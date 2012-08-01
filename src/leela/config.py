#!/usr/bin/python
# -*- coding: utf-8; -*-
#
# Copyright 2012 Juliano Martinez
# Copyright 2012 Diego Souza
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
import ConfigParser

def default_config_file():
    config = ConfigParser.ConfigParser()
    if ("LEELA_CFG" in os.environ):
        return(os.environ["LEELA_CFG"])
    else:
        return("/etc/leela/leela.conf")

def read_config(f=default_config_file()):
    cfg = ConfigParser.ConfigParser()
    cfg.read(f)
    return(cfg)
