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
  (:require [clojure.string :as s]
            [leela.blackbox.f :as f]
            [leela.blackbox.czmq.router :as router]
            [leela.blackbox.storage.cassandra :as storage]))

(defn msg-fail [status]
  ["fail" (str status)])

(defn msg-done []
  ["done"])

(defn msg-name [[u t n g]]
  (if-not g
    (msg-fail 404)
    ["name" u t n (str g)]))

(defn msg-link [links]
  (cons "link" (map str links)))

(defn msg-label [labels]
  (cons "label" labels))

(defn msg-nattr [attrs]
  (cons "n-attr" attrs))

(defn msg-tattr [msg]
  (cons "t-attr" (flatten (map (fn [[k v]] [(str k) v]) msg))))

(defn msg-kattr [data]
  ["k-attr" (if data data (byte-array 0))])

(defn exec-getname [cluster [g]]
  (let [g (f/bytes-to-uuid g)]
    (storage/with-consistency :one
      (storage/with-limit 1
        (if-let [[u t n] (storage/getname cluster g)]
          (msg-name [u t n g])
          (msg-name []))))))

(defn exec-getguid [cluster [u t k]]
  (let [u (f/bytes-to-str u)
        t (f/bytes-to-str t)
        k (f/bytes-to-str k)]
    (storage/with-consistency :one
      (msg-name [u t k (storage/getguid cluster u t k)]))))

(defn exec-putname [cluster [u t k]]
  (storage/with-consistency :quorum
    (let [u (f/bytes-to-str u)
          t (f/bytes-to-str t)
          k (f/bytes-to-str k)
          g (storage/putguid cluster u t k)]
      (msg-name [u t k g]))))

(defn exec-getlink [cluster [a l page & limit]]
  (let [a (f/bytes-to-uuid a)
        l (f/bytes-to-str l)
        page (if (empty? page) f/uuid-zero (f/bytes-to-uuid page))]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (msg-link (storage/getlink cluster a l page))))))

(defn exec-putlink [cluster links]
  (storage/with-consistency :one
    (storage/putlink
     cluster
     (map
      (fn [[a l b]] {:a (f/bytes-to-uuid a) :l (f/bytes-to-str l) :b (f/bytes-to-uuid b)})
      (partition 3 links))))
  (msg-done))

(defn exec-dellink [cluster links]
  (storage/with-consistency :one
    (storage/dellink
     cluster
     (map
      (fn [[a l b]]
        (if (empty? b)
          {:a (f/bytes-to-uuid a) :l (f/bytes-to-str l)}
          {:a (f/bytes-to-uuid a) :l (f/bytes-to-str l) :b (f/bytes-to-uuid b)}))
      (partition 3 links))))
  (msg-done))

(defn exec-get-tattr [cluster [k n & limit]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (msg-tattr (storage/get-tattr cluster k n))))))

(defn exec-put-tattr [cluster [k n s v]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)
        s (Integer. s)]
    (storage/with-consistency :one
      (storage/put-tattr cluster k n s v))
    (msg-done)))

(defn exec-del-tattr [cluster [k n s]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)
        s (Integer. s)]
    (storage/with-consistency :one
      (storage/del-tattr cluster k n s))
    (msg-done)))

(defn exec-get-kattr [cluster [k s]]
  (let [k (f/bytes-to-uuid k)
        s (f/bytes-to-str s)]
    (storage/with-consistency :one
      (msg-kattr (storage/get-kattr cluster k s)))))

(defn parse-opts [opt]
  (if (empty? opt)
    []
    (let [parse-funcs {"ttl" (fn [v] (Integer. v))}
          parse-keyval (fn [raw]
                         (let [[k v] (s/split raw #":" 2)]
                           [(keyword k) ((get parse-funcs k identity) v)]))]
      (mapcat parse-keyval (s/split opt #", ")))))

(defn exec-put-kattr [cluster attrs]
  (storage/with-consistency :one
    (storage/put-kattr
     cluster
     (map
      (fn [[k n v o]] [{:key (f/bytes-to-uuid k)
                        :name (f/bytes-to-str n)
                        :value (f/str-to-bytes v)}
                       (parse-opts (f/bytes-to-str o))])
        (partition 4 attrs))))
  (msg-done))

(defn exec-del-kattr [cluster attrs]
  (storage/with-consistency :one
    (storage/del-kattr
     cluster
     (map
      (fn [[k n]] {:key (f/bytes-to-uuid k) :name (f/bytes-to-str n)})
      (partition 2 attrs))))
  (msg-done))

(defn exec-getindex-exact [cluster table [k n]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)]
    (storage/with-consistency :one
      (storage/has-index cluster table k false n))))

(defn exec-getindex-all [cluster table [k page & limit]]
  (let [k (f/bytes-to-uuid k)
        page (f/bytes-to-str page)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/get-index cluster table k false page)))))

(defn exec-getindex-prefix [cluster table [k start finish & limit]]
  (let [k (f/bytes-to-uuid k)
        start (f/bytes-to-str start)
        finish (f/bytes-to-str finish)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/get-index cluster table k false start finish)))))

(defn exec-getindex-suffix [cluster table [k start finish & limit]]
  (let [k (f/bytes-to-uuid k)
        start (f/bytes-to-str start)
        finish (f/bytes-to-str finish)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/get-index cluster table k true start finish)))))

(defn exec-getlabel [cluster msg]
  (case (f/bytes-to-str (first msg))
    "all" (msg-label (exec-getindex-all cluster :g_index (drop 1 msg)))
    "pre" (msg-label (exec-getindex-prefix cluster :g_index (drop 1 msg)))
    "suf" (msg-label (exec-getindex-suffix cluster :g_index (drop 1 msg)))
    "ext" (msg-label (exec-getindex-exact cluster :g_index (drop 1 msg)))
    (msg-fail 400)))

(defn exec-listattr [cluster msg]
  (case (f/bytes-to-str (first msg))
    "all" (msg-nattr (exec-getindex-all cluster :p_index (drop 1 msg)))
    "pre" (msg-nattr (exec-getindex-prefix cluster :p_index (drop 1 msg)))
    "suf" (msg-nattr (exec-getindex-suffix cluster :p_index (drop 1 msg)))
    "ext" (msg-nattr (exec-getindex-exact cluster :p_index (drop 1 msg)))))

(defn exec-putlabel [cluster labels]
  (storage/with-consistency :one
    (storage/put-index
     cluster
     :g_index
     (map (fn [[k n]] {:key (f/bytes-to-uuid k) :name (f/bytes-to-str n)})
          (partition 2 labels))))
  (msg-done))

(defn handle-get [cluster msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-getname cluster (drop 1 msg))
    "guid" (exec-getguid cluster (drop 1 msg))
    "link" (exec-getlink cluster (drop 1 msg))
    "label" (exec-getlabel cluster (drop 1 msg))
    "attr" (exec-listattr cluster (drop 1 msg))
    "t-attr" (exec-get-tattr cluster (drop 1 msg))
    "k-attr" (exec-get-kattr cluster (drop 1 msg))
    (msg-fail 400)))

(defn handle-put [cluster msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-putname cluster (drop 1 msg))
    "link" (exec-putlink cluster (drop 1 msg))
    "label" (exec-putlabel cluster (drop 1 msg))
    "t-attr" (exec-put-tattr cluster (drop 1 msg))
    "k-attr" (exec-put-kattr cluster (drop 1 msg))
    (msg-fail 400)))

(defn handle-del [cluster msg]
  (case (f/bytes-to-str (first msg))
    "link" (exec-dellink cluster (drop 1 msg))
    "t-attr" (exec-del-tattr cluster (drop 1 msg))
    "k-attr" (exec-del-kattr cluster (drop 1 msg))
    (msg-fail 400)))

(defn handle-message [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (case (f/bytes-to-str (first msg))
      "get" (handle-get cluster (drop 1 msg))
      "put" (handle-put cluster (drop 1 msg))
      "del" (handle-del cluster (drop 1 msg))
      (msg-fail 400))))

(defn zmqworker [cluster]
  {:onjob #(handle-message cluster %) :onerr (msg-fail 500)})

(defn server-start [ctx cluster options]
  (router/router-start ctx (zmqworker cluster) options))
