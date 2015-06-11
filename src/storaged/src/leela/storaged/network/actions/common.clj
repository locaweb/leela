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
