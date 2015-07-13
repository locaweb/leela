;; Copyright (C) 2015  Diego Souza

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(ns leela.storaged.security
  (:import
   java.nio.ByteBuffer)
  (:require
   [pandect.core :refer [sha1-bytes sha1-hmac-bytes]]
   [clj-time.core :refer [now]]
   [clj-time.coerce :refer [to-long]]))

(def ^:dynamic *secret*)

(def ^:dynamic *time-window-in-ms* 300000)

(defmacro with-secret [secret & body]
  `(binding [*secret* ~secret]
     ~@body))

(defmacro with-time-window-in-ms [w & body]
  `(binding [*time-window-in-ms* w]
     ~@body))

(def signature-size (count (sha1-bytes "")))

(defn signature [^ByteBuffer payload]
  (let [msg (byte-array (.remaining payload))]
    (.get payload msg)
    (sha1-hmac-bytes msg *secret*)))

(defn signature? [^ByteBuffer payload]
  (if (>= (.remaining payload) signature-size)
    (let [sig (byte-array (min (.remaining payload) signature-size))]
      (.get payload sig)
      (= (seq sig) (seq (signature payload))))
    false))

(defn timestamp? [time ^ByteBuffer payload]
  (if (>= (.remaining payload) 8)
    (let [p-time (.getLong payload)]
      (and (> p-time (- time *time-window-in-ms*))
           (< p-time (+ time *time-window-in-ms*))))
    false))

(defn check-signature! [^ByteBuffer payload]
  (when (not (and (nil? payload) (signature? (.slice payload))))
    (let [tmp (byte-array signature-size)]
      (.get payload tmp))
    payload))

(defn check-timestamp! [time ^ByteBuffer payload]
  (when (not (and (nil? payload) (timestamp? time (.slice payload))))
    (.getLong payload)
    payload))

(defn authenticate-with-time [time-now ^ByteBuffer payload]
  (->> (check-signature! payload)
       (check-timestamp! time-now)))

(defn authenticate [^ByteBuffer payload]
  (authenticate-with-time (to-long (now)) payload))
