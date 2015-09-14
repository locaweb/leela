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

(ns leela.storaged.control
  (:require
   [clojure.tools.logging :refer [error]]))

(defmacro forever [& body]
  `(while true ~@body))

(defmacro try-fn [& f]
  `(try
    [:r ~@f]
    (catch Exception e#
      [:l e#])))

;; This function issues a warning: recur arg for primitive local: n is
;; not matching primitive. At the time we wrote this, it was still an
;; open issue: http://dev.clojure.org/jira/browse/CLJ-701
(defmacro supervise [& fn]
  `(loop [n# 0]
     (let [v# (try-fn ~@fn)
           s# (reduce * (repeat n# 2))]
       (case (first v#)
         :r (second v#)
         :l (do
              (error (second v#) (format "supervised function has died; waiting %d seconds" s#))
              (Thread/sleep (* 1000 s#))
              (recur (mod (inc n#) 300)))))))
