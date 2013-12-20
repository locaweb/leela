;; Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
;; Copyright 2013 (c) Alexandre Baaklini <abaaklini@gmail.com>
;;  
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;  
;;     http://www.apache.org/licenses/LICENSE-2.0
;;  
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns leela.blackbox.data.time
  (:use     [clj-time.format :only [formatter
                                    parse
                                    unparse]]
            [clj-time.coerce :only [to-long
                                    from-long]]))

(def custom-formatter (formatter "yyyyMMddhhmmss"))

(defn str-date-to-long
  "Given a string in the following format yyyyMMddhhmmss
  returns a long value to be stored on the database"
  [s]
  (to-long (parse custom-formatter s)))

(defn long-to-date-str
  "Given a long number returns the respective string in
  the following format yyyyMMddhhmmss"
  [l]
  (unparse custom-formatter (from-long l)))
