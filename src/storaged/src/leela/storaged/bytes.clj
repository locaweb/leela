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
   java.nio.ByteBuffer
   org.apache.commons.codec.binary.Base64)
  (:require
   [clojure.string :refer [join]]))

(defn bytes= [xs ys]
  (= (seq xs) (seq ys)))

(defn bytes-from-chars [xs]
  (byte-array (map byte xs)))

(defn bstr-from-chars [xs]
  (let [len (count xs)]
    (if (< len 256)
      (byte-array (cons (count xs) (map byte xs))))))

(defn chars-from-bytes [xs]
  (join (map char xs)))

(defn chars-from-bstr [xs]
  (if-let [len (bit-and (first xs) 0xFF)]
    (let [[left, right] (split-at len (rest xs))]
      (list (chars-from-bytes left) right))))

(defn chars-from-bstr-only [xs]
  (first (chars-from-bstr xs)))

(defn bytes-from-bytebuff [^ByteBuffer src]
  (let [dst (byte-array (.remaining src))]
    (.get src dst)
    dst))

(defn bytebuff-from-bytes (^ByteBuffer [xs]
  (ByteBuffer/wrap xs)))

(defn bytes-from-long [x]
  (let [buff (byte-array 8)]
    (.putLong (bytebuff-from-bytes buff) x)
    buff))

(defn long-from-bytes [xs]
  (let [bbuf (bytebuff-from-bytes xs)
        num  (.getLong bbuf)]
    (list num (bytes-from-bytebuff bbuf))))

(defn long-from-bytes-only [xs]
  (first (long-from-bytes xs)))

(defn byte-array? [x]
  (let [ty (type (byte-array 0))]
    (instance? ty x)))

(defn bytes-from-base64 [x]
  (Base64/decodeBase64 x))

(defn base64-from-bytes [^bytes x]
  (Base64/encodeBase64 x false))

(defn base64? [x]
  (Base64/isBase64 x))

(defn concat-bytes [a b]
  (into-array Byte/TYPE (concat (seq a) (seq b))))
