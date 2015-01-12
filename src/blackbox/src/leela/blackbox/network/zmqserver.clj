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
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.storage.s3 :as s3]))

(defn msg-fail [status]
  ["fail" (str status)])

(defn msg-done []
  ["done"])

(defn msg-name [u t k n g]
  (if-not g
    (msg-fail 404)
    ["name" u t k n (str g)]))

(defn msg-link [links]
  (cons "link" (map str links)))

(defn msg-label [labels]
  (cons "label" labels))

(defn msg-nattr [attrs]
  (cons "n-attr" attrs))

(defn msg-tattr [data]
  (cons "t-attr" (flatten (map (fn [[k v]] [(str k) v]) data))))

(defn msg-kattr [data]
  (if-not data
    (msg-fail 404)
    ["k-attr" data]))

(defn parse-opts [opt]
  (if (empty? opt)
    {}
    (let [parse-funcs {"ttl" (fn [v] (Integer. v))
                       "index" (fn [v] (boolean v))}
          parse-keyval (fn [raw]
                         (let [[k v] (s/split raw #":" 2)]
                           [(keyword k) ((get parse-funcs k identity) v)]))]
      (apply (partial assoc {}) (mapcat parse-keyval (s/split opt #", "))))))

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

(defn exec-get-tattr [cluster [k n t & limit]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)
        t (Long. (f/bytes-to-str t))]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
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
          :time (Long. (f/bytes-to-str t))
          :value (f/str-to-bytes v)}
         (parse-opts (f/bytes-to-str o))])
        (partition 5 attrs))))
  (msg-done))

(defn exec-del-tattr [cluster attrs]
  (storage/with-consistency :one
    (storage/del-tattr
     cluster
     (map
      (fn [[k n t]] {:key (f/bytes-to-uuid k)
                     :name (f/bytes-to-str n)
                     :time (Long. (f/bytes-to-str t))})
      (partition 3 attrs))))
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
      (storage/has-index cluster table k n))))

(defn exec-getindex-all [cluster table [k page & limit]]
  (let [k (f/bytes-to-uuid k)
        page (f/bytes-to-str page)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/get-index cluster table k page)))))

(defn exec-getindex-prefix [cluster table [k start finish & limit]]
  (let [k (f/bytes-to-uuid k)
        start (f/bytes-to-str start)
        finish (f/bytes-to-str finish)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/get-index cluster table k start finish)))))

(defn exec-getlabel [cluster msg]
  (case (f/bytes-to-str (first msg))
    "all" (msg-label (exec-getindex-all cluster :g_index (drop 1 msg)))
    "pre" (msg-label (exec-getindex-prefix cluster :g_index (drop 1 msg)))
    "ext" (msg-label (exec-getindex-exact cluster :g_index (drop 1 msg)))
    (msg-fail 400)))

(defn exec-listattr [cluster msg]
  (let [table (get {"k-attr" :k_index
                    "t-attr" :t_index} (f/bytes-to-str (first msg)))]
    (case (f/bytes-to-str (second msg))
      "all" (msg-nattr (exec-getindex-all cluster table (drop 2 msg)))
      "pre" (msg-nattr (exec-getindex-prefix cluster table (drop 2 msg)))
      "ext" (msg-nattr (exec-getindex-exact cluster table (drop 2 msg))))))

(defn exec-putlabel [cluster labels]
  (storage/with-consistency :one
    (storage/put-index
     cluster
     :g_index
     (map (fn [[k n]] {:key (f/bytes-to-uuid k) :name (f/bytes-to-str n)})
          (partition 2 labels))))
  (msg-done))

(defn handle-get [attr-cluster graph-cluster s3-cred msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-getname graph-cluster (drop 1 msg))
    "guid" (exec-getguid graph-cluster (drop 1 msg))
    "link" (exec-getlink graph-cluster (drop 1 msg))
    "label" (exec-getlabel graph-cluster (drop 1 msg))
    "attr" (exec-listattr attr-cluster (drop 1 msg))
    "t-attr" (exec-get-tattr attr-cluster (drop 1 msg))
    "at-attr" (exec-get-archived-tattr s3-cred (drop 1 msg))
    "k-attr" (exec-get-kattr attr-cluster (drop 1 msg))
    (msg-fail 400)))

(defn handle-put [attr-cluster graph-cluster s3-cred msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-putname graph-cluster (drop 1 msg))
    "link" (exec-putlink graph-cluster (drop 1 msg))
    "label" (exec-putlabel graph-cluster (drop 1 msg))
    "t-attr" (exec-put-tattr attr-cluster (drop 1 msg))
    "at-attr" (exec-put-archived-tattr s3-cred (drop 1 msg))
    "at-bucket" (exec-put-archived-bucket s3-cred (drop 1 msg))
    "k-attr" (exec-put-kattr attr-cluster (drop 1 msg))
    (msg-fail 400)))

(defn handle-del [attr-cluster graph-cluster s3-cred msg]
  (case (f/bytes-to-str (first msg))
    "link" (exec-dellink graph-cluster (drop 1 msg))
    "t-attr" (exec-del-tattr attr-cluster (drop 1 msg))
    "k-attr" (exec-del-kattr attr-cluster (drop 1 msg))
    (msg-fail 400)))

(defn handle-message [attr-cluster graph-cluster s3-cred msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (case (f/bytes-to-str (first msg))
      "get" (handle-get attr-cluster graph-cluster s3-cred (drop 1 msg))
      "put" (handle-put attr-cluster graph-cluster s3-cred (drop 1 msg))
      "del" (handle-del attr-cluster graph-cluster s3-cred (drop 1 msg))
      (msg-fail 400))))

(defn zmqworker [attr-cluster graph-cluster s3-cred]
  {:onjob #(handle-message attr-cluster graph-cluster s3-cred %) :onerr (msg-fail 500)})

(defn server-start [ctx attr-cluster graph-cluster s3-cred options]
  (router/router-start ctx (zmqworker attr-cluster graph-cluster s3-cred) options))
