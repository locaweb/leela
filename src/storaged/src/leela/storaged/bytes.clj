;; Copyright (c) 2015 <Diego Souza>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

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

(defn byte-array? [x]
  (let [ty (type (byte-array 0))]
    (instance? ty x)))

(defn bytebuffer-from-string [s]
  (ByteBuffer/wrap (bytes-from-chars s)))

(defn concat-bytes [a b]
  (into-array Byte/TYPE (concat (seq a) (seq b))))
