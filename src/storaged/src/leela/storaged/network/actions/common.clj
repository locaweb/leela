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

(ns leela.storaged.network.actions.common)

(def default-limit 100)

(defn fail [code msg]
  [:fail code msg])

(defn nil-or [pred]
  #(or (nil? %) (pred %)))

;; (defn- arg-errors [args tests]
;;   (filter #(%1 %2) (map vector tests args)))

(defn map-errors [test data]
  (letfn [(valid-fn [[key val]]
            (or (not (contains? test key)) ((get test key) val)))]
    (if-not (map? data)
      [":" "type-error"]
      (first (filter (complement valid-fn) (seq data))))))

(defn arg-errors [test data]
  (if-not (coll? data)
    [-1 "type-error"]
    (map-errors test (into {} (map-indexed vector data)))))

(defmacro when-map [tests params & body]
  `(if-let [e# (map-errors ~tests ~params)]
     (fail 400 (format "invalid param: data[%s]=%s" (first e#) (pr-str (second e#))))
     (do ~@body)))

(defmacro when-args [tests args & body]
  `(if-let [e# (arg-errors ~tests ~args)]
     (fail 400 (format "invalid argument: data[%d]=%s" (first e#) (pr-str (second e#))))
     (do ~@body)))
