;; Copyright 2015 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.storaged.bytes
  (:import
   java.nio.ByteBuffer)
  (:require
   [clojure.string :refer [join]]))

(defn bytes-from-chars [xs]
  (byte-array (map byte xs)))

(defn chars-from-bytes [xs]
  (join (map char xs)))

(defn bytes-from-bytebuff [^ByteBuffer src]
  (let [dst (byte-array (.remaining src))]
    (.get src dst)
    dst))
