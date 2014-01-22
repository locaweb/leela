#  Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
#   
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#   
#      http://www.apache.org/licenses/LICENSE-2.0
#   
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

class LeelaException(Exception):
  def __init__(self, msg, ercode):
    super(LeelaException, self).__init__(msg)
    self.ercode = ercode

class UserException(LeelaException):
  def __init__(self, msg, ercode):
    super(UserException, self).__init__(msg, ercode)

class ServerException(LeelaException):
  def __init__(self, msg, ercode):
    super(ServerException, self).__init__(msg, ercode)

class BadRequest(UserException):
  def __init__(self, msg, ercode):
    super(BadRequest, self).__init__(msg, ercode)

class Forbidden(UserException):
  def __init__(self, msg, ercode):
    super(Forbidden, self).__init__(msg, ercode)

class NotFound(UserException):
  def __init__(self, msg, ercode):
    super(NotFound, self).__init__(msg, ercode)

class InternalServerError(ServerException):
  def __init__(self, msg, ercode):
    super(InternalServerError, self).__init__(msg, ercode)
