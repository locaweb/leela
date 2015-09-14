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

(ns leela.storaged.time
  (:require
   [clj-time.core :refer [now]]
   [clj-time.coerce :refer [to-long]]))

(defn monotonic-time []
  (System/nanoTime))

(defmacro time-it [& f]
  `(let [start# (monotonic-time)
         reply# ~@f
         stop#  (monotonic-time)]
     [(- stop# start#) reply#]))

(defn human-string [d]
  (let [umetrics [[1000 "ns"] [1000 "us"] [1000 "ms"] [60 "s"] [60 "m"]]]
    (loop [n            d
           [[q u] & us] umetrics]
      (if (and (>= n q) (not-empty us))
        (recur (/ n q) us)
        (format "%.3f %s" (float n) u)))))

(defn time-now []
  (now))

(defn timestamp-now []
  (to-long (now)))
