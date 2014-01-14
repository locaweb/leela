;; Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.blackbox.f
  (:use     [clojure.tools.logging :only [error]]
            [clojure.inspector])
  (:import  java.util.UUID
            com.datastax.driver.core.utils.UUIDs)
  (:require [clojure.data.json :as json]))

(defmacro forever [& body]
  `(while true ~@body))

(defmacro forever-with [check & body]
  `(while (~check) ~@body))

; http://stackoverflow.com/questions/1879885/clojure-how-to-to-recur-upon-exception
(defn retry-on-error1 [n f]
  (loop [n n]
    (if-let [result (try
                      [(f)]
                      (catch Exception e
                        (when (zero? n)
                          (throw e))))]
      (result 0)
      (recur (dec n)))))

(defmacro retry-on-error [n & body]
  `(retry-on-error1 ~n (fn [] ~@body)))

(defmacro supervise [& body]
  `(supervise-with (fn [] true) ~@body))

(defmacro supervise-with [check & body]
  `(forever-with ~check
    (try
      ~@body
      (catch Exception e#
        (error e# "supervised function has died, restarting")
        (Thread/sleep 500)))))

(def +charset-ascii+ (.get (java.nio.charset.Charset/availableCharsets) "US-ASCII"))

(def json-to-str json/write-str)

(def uuid-zero (UUIDs/startOf 0))

(defmacro str-to-json [s]
  `(json/read-str ~s :key-fn keyword))

(defmacro uuid-1 []
  `(UUIDs/timeBased))

(defmacro str-to-bytes [s]
  `(if (instance? String ~s)
     (.getBytes ~s +charset-ascii+)
     ~s))

(defn bytes-to-str [bytes]
  (if (instance? String bytes)
    bytes
    (apply str (map char bytes))))

(defn maybe-bytes-to-str [bytes]
  (when bytes (bytes-to-str bytes)))

(defn binary-to-bytes [b]
  (let [buffer (byte-array (.remaining b))]
    (.get b buffer)
    buffer))

(defmacro str-to-uuid [s]
  `(UUID/fromString ~s))

(defmacro bytes-to-uuid [b]
  `(str-to-uuid (bytes-to-str ~b)))
