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

(ns leela.storaged.network.protocol
  (:import
   [java.util Base64])
  (:require
   [msgpack.core :as msgp]
   [clojure.string :refer [split]]
   [slingshot.slingshot :refer [throw+]]
   [leela.storaged.bytes :refer :all]
   [leela.storaged.security :refer [sign]]))
   
(defn encode-query
  ([code hdrs body]
   ["query-1.0" code hdrs body])
  ([code hdrs]
   ["query-1.0" code hdrs nil])
  ([code]
   ["query-1.0" code {} nil]))

(defn encode-reply
  ([code hdrs body]
   ["reply-1.0" code hdrs body])
  ([code hdrs]
   ["reply-1.0" code hdrs nil])
  ([code]
   ["reply-1.0" code {} nil]))

(defn kind [q-o-r]
  (nth q-o-r 0))

(defn kind-nover [q-o-r]
  (let [known {"reply-1.0" :reply
               "query-1.0" :query}]
    (get known (kind q-o-r))))

(defn status [q-o-r]
  (nth q-o-r 1))

(defn headers [q-o-r]
  (nth q-o-r 2))

(defn payload [q-o-r]
  (nth q-o-r 3))

(defn payload-fmap [fun q-o-r]
    [(kind q-o-r) (status q-o-r) (headers q-o-r) (fun (payload q-o-r))])

(defn headers-fmap [fun q-o-r]
  [(kind q-o-r) (status q-o-r) (fun (headers q-o-r)) (payload q-o-r)])

(defn query? [msg]
  (= :query (kind-nover msg)))

(defn reply? [msg]
  (= :reply (kind-nover msg)))

(defn header
  ([q-o-r key default]
   (get (headers q-o-r) key default))
  ([q-o-r key]
   (get (headers q-o-r) key)))

(defn header-content-type [q-o-r]
  (header q-o-r :content-type ["application/x-msgpack"]))

(defn header-accept [q-o-r]
  (header q-o-r :accept ["application/x-msgpack"]))

(defn- unframe-tr [msg]
  (cond
    (map? msg) (into {} (for [[k v] msg] [(keyword k) (unframe-tr v)]))
    (coll? msg) (map unframe-tr msg)
    true msg))

(defn unframe [^bytes frame]
  (unframe-tr (msgp/unpack frame)))

(defn unframe-when [frame vfun]
  (if-let [q-o-r (unframe frame)]
    (when (vfun q-o-r)
      q-o-r)))

(defn- keyword->str [k]
  (subs (str k) 1))

(defn- frame-tr [msg]
  (cond
    (map? msg) (into {} (for [[k v] msg] [(frame-tr k) (frame-tr v)]))
    (coll? msg) (map frame-tr msg)
    (keyword? msg) (subs (str msg) 1)
    true msg))

(defn frame [message]
  (msgp/pack (frame-tr message)))

