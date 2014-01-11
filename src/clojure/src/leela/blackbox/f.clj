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

;; (defn random-string [bits]
;;   (.toString (java.math.BigInteger. bits (java.security.SecureRandom.)) 32))

(def +charset-ascii+ (.get (java.nio.charset.Charset/availableCharsets) "US-ASCII"))

(defn bytes-to-str [bytes]
  (if (instance? String bytes)
    bytes
    (apply str (map char bytes))))

(defmacro str-to-json [s]
  `(json/read-str ~s :key-fn keyword))

(def json-to-str json/write-str)

(defn str-to-bytes [s]
  (.getBytes s +charset-ascii+))

(defn hexstr-to-binary [s]
  (org.apache.commons.codec.binary.Hex/decodeHex (.toCharArray s)))

(defn bytes-to-binary [b]
  (org.apache.commons.codec.binary.Hex/decodeHex (char-array (map char b))))

(defn binary-to-hexstr [b]
  (let [buffer (byte-array (.remaining b))]
    (.get b buffer)
    (org.apache.commons.codec.binary.Hex/encodeHexString buffer)))

(def b-0x00 (bytes-to-binary "00"))
(def b-0x01 (bytes-to-binary "01"))
(def b-0x02 (bytes-to-binary "02"))
(def b-0x03 (bytes-to-binary "03"))
(def b-0x04 (bytes-to-binary "04"))
(def b-0x (bytes-to-binary ""))

(defn bin-0x00 [] (java.nio.ByteBuffer/wrap (byte-array [(byte 0)])))
(defn bin-0x01 [] (java.nio.ByteBuffer/wrap (byte-array [(byte 1)])))
(defn bin-0x02 [] (java.nio.ByteBuffer/wrap (byte-array [(byte 2)])))
(defn bin-0x03 [] (java.nio.ByteBuffer/wrap (byte-array [(byte 3)])))
(defn bin-0x04 [] (java.nio.ByteBuffer/wrap (byte-array [(byte 4)])))
(defn bin-0x [] (java.nio.ByteBuffer/wrap (byte-array [])))
