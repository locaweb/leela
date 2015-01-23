;; Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.blackbox.network.zmqserver
  (:use     [clojure.tools.logging :only [trace debug info error]])
  (:require [msgpack.core :as msgp]
            [leela.blackbox.f :as f]
            [leela.blackbox.czmq.router :as router]
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.storage.s3 :as s3]))

(defmacro third [list]
  `(nth ~list 2))

(defmacro parse-opts [opts]
  `(into {} (for [[k# v#] ~opts] [(keyword k#) v#])))

(def msg-pack-magic "000")

(defn msg-fail [status]
  ["fail" status])

(defn msg-done []
  ["done" nil])

(defn msg-name [u t k n g]
  (if-not g
    (msg-fail 404)
    ["name" [u t k n (str g)]]))

(defn msg-link [links]
  ["link" (map str links)])

(defn msg-label [labels]
  ["label" labels])

(defn msg-nattr [attrs]
  ["n-attr" attrs])

(defn msg-tattr [data]
  ["t-attr" data])

(defn msg-kattr [data]
  (if-not data
    (msg-fail 404)
    ["k-attr" data]))

(defn exec-getname [cluster [g]]
  (let [g (f/bytes-to-uuid g)]
    (storage/with-consistency :one
      (storage/with-limit 1
        (if-let [[u t k n] (storage/getname cluster g)]
          (msg-name u t k n g)
          (msg-fail 404))))))

(defn exec-getguid [cluster [u t k n]]
  (let [u (f/bytes-to-str u)
        t (f/bytes-to-str t)
        k (f/bytes-to-str k)
        n (f/bytes-to-str n)]
    (storage/with-consistency :one
      (storage/with-limit 1
        (msg-name u t k n (storage/getguid cluster u t k n))))))

(defn exec-putname [cluster [u t k n]]
  (storage/with-consistency :quorum
    (let [u (f/bytes-to-str u)
          t (f/bytes-to-str t)
          k (f/bytes-to-str k)
          n (f/bytes-to-str n)
          g (storage/putguid cluster u t k n)]
      (msg-name u t k n g))))

(defn exec-getlink [cluster [a l page] limit]
  (let [a (f/bytes-to-uuid a)
        l (f/bytes-to-str l)
        page (if (empty? page) f/uuid-zero (f/bytes-to-uuid page))]
    (storage/with-consistency :one
      (storage/with-limit limit
        (msg-link (storage/getlink cluster a l page))))))

(defn exec-putlink [cluster links]
  (storage/with-consistency :one
    (storage/putlink
     cluster
     (map
      (fn [[a l b]] {:a (f/bytes-to-uuid a) :l (f/bytes-to-str l) :b (f/bytes-to-uuid b)}) links)))
  (msg-done))

(defn exec-dellink [cluster links]
  (storage/with-consistency :one
    (storage/dellink
     cluster
     (map
      (fn [[a l b]]
        (if (empty? b)
          {:a (f/bytes-to-uuid a) :l (f/bytes-to-str l)}
          {:a (f/bytes-to-uuid a) :l (f/bytes-to-str l) :b (f/bytes-to-uuid b)})) links)))
  (msg-done))

(defn exec-get-tattr [cluster [k n t] limit]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)]
    (storage/with-consistency :one
      (storage/with-limit limit
        (msg-tattr (storage/get-tattr cluster k n t))))))

(defn exec-get-archived-tattr [s3-cred [b a]]
  (let [b (f/bytes-to-str b)
        a (f/bytes-to-str a)]
    (f/stream-to-bytes (s3/get-archived-tattr s3-cred b a))))

(defn exec-put-archived-tattr [s3-cred [b a v]]
  (let [b (f/bytes-to-str b)
        a (f/bytes-to-str a)]
    (let [v (s3/put-archived-tattr s3-cred b a v)]
      (if-not (:errorcode v)
        (msg-done)
        (msg-fail (:statuscode v))))))

(defn exec-put-archived-bucket [s3-cred [b]]
  (let [b (f/bytes-to-str b)]
    (s3/create-bucket s3-cred b))
  (msg-done))

(defn exec-put-tattr [cluster attrs]
  (storage/with-consistency :one
    (storage/put-tattr
     cluster
     (map
      (fn [[k n t v o]]
        [{:key (f/bytes-to-uuid k)
          :name (f/bytes-to-str n)
          :time t
          :value (f/str-to-bytes v)}
         (parse-opts o)]) attrs)))
  (msg-done))

(defn exec-del-tattr [cluster attrs]
  (storage/with-consistency :one
    (storage/del-tattr
     cluster
     (map
      (fn [[k n t]] {:key (f/bytes-to-uuid k)
                     :name (f/bytes-to-str n)
                     :time t}) attrs)))
  (msg-done))

(defn exec-get-kattr [cluster [k s]]
  (let [k (f/bytes-to-uuid k)
        s (f/bytes-to-str s)]
    (storage/with-consistency :one
      (msg-kattr (storage/get-kattr cluster k s)))))

(defn exec-put-kattr [cluster attrs]
  (storage/with-consistency :one
    (storage/put-kattr
     cluster
     (map
      (fn [[k n v o]] [{:key (f/bytes-to-uuid k)
                        :name (f/bytes-to-str n)
                        :value (f/str-to-bytes v)}
                       (parse-opts o)]) attrs)))
  (msg-done))

(defn exec-del-kattr [cluster attrs]
  (storage/with-consistency :one
    (storage/del-kattr
     cluster
     (map
      (fn [[k n]] {:key (f/bytes-to-uuid k) :name (f/bytes-to-str n)}) attrs)))
  (msg-done))

(defn exec-getindex-exact [cluster table [k n]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)]
    (storage/with-consistency :one
      (storage/has-index cluster table k n))))

(defn exec-getindex-all [cluster table [k page] limit]
  (let [k (f/bytes-to-uuid k)
        page (f/bytes-to-str page)]
    (storage/with-consistency :one
      (storage/with-limit limit
        (storage/get-index cluster table k page)))))

(defn exec-getindex-prefix [cluster table [k start finish] limit]
  (let [k (f/bytes-to-uuid k)
        start (f/bytes-to-str start)
        finish (f/bytes-to-str finish)]
    (storage/with-consistency :one
      (storage/with-limit limit
        (storage/get-index cluster table k start finish)))))

(defn exec-getlabel [cluster msg limit]
  (case (first msg)
    "all" (msg-label (exec-getindex-all cluster :g_index (drop 1 msg) limit))
    "ext" (msg-label (exec-getindex-exact cluster :g_index (drop 1 msg)))
    "pre" (msg-label (exec-getindex-prefix cluster :g_index (drop 1 msg) limit))
    (msg-fail 400)))

(defn exec-enum [cluster [t l & attrs]]
    (let [t (Long. (f/bytes-to-str t))
          l (f/bytes-to-str l)]
      (if-let [[k n b] attrs]
        (do
          (let [k (f/bytes-to-uuid k)
                n (f/bytes-to-str n)
                b (f/bytes-to-str b)]
          (storage/enum-tattr cluster t l k n b)))
        (storage/enum-tattr cluster t l))))

(defn exec-listattr [cluster table msg limit]
  (let [table (get {"k-attr" :k_index "t-attr" :t_index} table)]
    (case (first msg)
      "all" (msg-nattr (exec-getindex-all cluster table (drop 1 msg) limit))
      "ext" (msg-nattr (exec-getindex-exact cluster table (drop 1 msg)))
      "pre" (msg-nattr (exec-getindex-prefix cluster table (drop 1 msg) limit))
      (msg-fail 400))))

(defn exec-putlabel [cluster labels]
  (storage/with-consistency :one
    (storage/put-index
     cluster
     :g_index
     (map (fn [[k n]] {:key (f/bytes-to-uuid k) :name (f/bytes-to-str n)}) labels)))
  (msg-done))

(defn handle-get [attr-cluster graph-cluster msg]
  (case (first msg)
    "attr" (let [args (drop 1 msg)]
             (exec-listattr attr-cluster (first args) (second args) (third args)))
    "guid" (exec-getguid graph-cluster (second msg))
    "link" (exec-getlink graph-cluster (second msg) (third msg))
    "name" (exec-getname graph-cluster (second msg))
    "label" (exec-getlabel graph-cluster (second msg) (third msg))
    "k-attr" (exec-get-kattr attr-cluster (second msg))
    "t-attr" (exec-get-tattr attr-cluster (second msg) (third msg))
    (msg-fail 400)))

(defn handle-put [attr-cluster graph-cluster msg]
  (case (first msg)
    "link" (exec-putlink graph-cluster (second msg))
    "name" (exec-putname graph-cluster (second msg))
    "label" (exec-putlabel graph-cluster (second msg))
    "k-attr" (exec-put-kattr attr-cluster (second msg))
    "t-attr" (exec-put-tattr attr-cluster (second msg))
    (msg-fail 400)))

(defn handle-del [attr-cluster graph-cluster msg]
  (case (first msg)
    "link" (exec-dellink graph-cluster (second msg))
    "k-attr" (exec-del-kattr attr-cluster (second msg))
    "t-attr" (exec-del-tattr attr-cluster (second msg))
    (msg-fail 400)))

(defn fmtreply [req rep]
  (case (first rep)
    "fail" [[(take 2 req) rep] [msg-pack-magic (msgp/pack rep)]]
    [[(take 2 req) (first rep)] [msg-pack-magic (msgp/pack rep)]]))

(defn handle-message [attr-cluster graph-cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (case (first msg)
      "del" (handle-del attr-cluster graph-cluster (drop 1 msg))
      "get" (handle-get attr-cluster graph-cluster (drop 1 msg))
      "put" (handle-put attr-cluster graph-cluster (drop 1 msg))
      (msg-fail 400))))

(defn handle-message-frame [attr-cluster graph-cluster msg]
  (if (not= (count msg) 2)
    (fmtreply [] (msg-fail 400))
    (if (= msg-pack-magic (f/bytes-to-str (first msg)))
      (let [req (msgp/unpack (second msg))]
        (fmtreply req (handle-message attr-cluster graph-cluster req)))
      (fmtreply [] (msg-fail 400)))))

(defn zmqworker [attr-cluster graph-cluster]
  {:onjob #(handle-message-frame attr-cluster graph-cluster %) :onerr (msg-fail 500)})

(defn server-start [ctx attr-cluster graph-cluster options]
  (router/router-start ctx (zmqworker attr-cluster graph-cluster) options))
