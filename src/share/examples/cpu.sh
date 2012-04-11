#!/bin/bash
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

statgrab | awk '/cpu/ {
    if (!data){
        data = ""
    };
    gsub(/^cpu\./,"",$1);
    data = $1"|"$3"||"data
    } END {
        gsub(/\|\|$/,"",data);
        print "'$(uname -n)'|cpu||"data
}' | bzip2 -9 -c | nc -w 2 -u 127.0.0.1 6968
