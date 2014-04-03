;; Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.blackbox.security.authentication
  (:use     [clj-time.core   :only [minutes
                                    ago
                                    from-now
                                    within?
                                    interval]]
            [clj-time.coerce :only [to-long
                                    from-long]])
  (:require [pandect.core   :as auth]))

(defn signature
  "With a message and secret provided by the caller,
  generate the MD5 HMAC hash."
  [message secret]
  (auth/md5-hmac message secret))

(defn valid-time?
  "Given a timestamp, a specific moment on time and
  a window around this moment,
  checks if the timestamp was within this interval."
  [timestamp now window]
  (within?
    (interval
      (from-long (- (to-long (now)) (* window 60000)))
      (from-long (+ (to-long (now)) (* window 60000))))
    timestamp))

(defn valid-nonce?
  "Given a nonce, checks if it's valid."
  [nonce]
  (true))
