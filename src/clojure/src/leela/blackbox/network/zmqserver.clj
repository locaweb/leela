;; Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

(def +index-name+ 0x00)
(def +index-pxlabel+ 0x01)
(def +index-sxlabel+ 0x02)

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

(defn msg-tattr [msg]
  (cons "t-attr" (flatten (map (fn [[k v]] [(str k) v]) msg))))

(defn msg-kattr [data]
  ["k-attr" (if data data (byte-array 0))])

(defn exec-getname [cluster [g]]
  (let [g (f/bytes-to-uuid g)]
    (storage/with-consistency :one
      (storage/with-limit 1
        (if-let [data (first (storage/getindex cluster g +index-name+))]
          (msg-name (conj (f/str-to-json data) g))
          (msg-name []))))))

(defn exec-getguid [cluster [u t k]]
  (let [u (f/bytes-to-str u)
        t (f/bytes-to-str t)
        k (f/bytes-to-str k)]
    (storage/with-consistency :one
      (msg-name [u t k (storage/getguid cluster u t k)]))))

(defn exec-putname [cluster [u t k]]
  (let [u (f/bytes-to-str u)
        t (f/bytes-to-str t)
        k (f/bytes-to-str k)
        g (storage/putguid cluster u t k)]
    (storage/with-consistency :quorum
      (storage/putindex cluster g +index-name+ (f/json-to-str [u t k]))
      (msg-name [u t k g]))))

(defn exec-getlink [cluster [a l page & limit]]
  (let [a (f/bytes-to-uuid a)
        l (f/bytes-to-str l)
        page (if (empty? page) f/uuid-zero (f/bytes-to-uuid page))]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (msg-link (storage/getlink cluster a l page))))))

(defn exec-putlink [cluster [a l b]]
  (let [a (f/bytes-to-uuid a)
        l (f/bytes-to-str l)
        b (f/bytes-to-uuid b)]
    (storage/with-consistency :quorum
      (storage/putlink cluster a l b)
      (msg-done))))

(defn exec-dellink [cluster [a l & b]]
  (let [a (f/bytes-to-uuid a)
        l (f/bytes-to-str l)]
    (storage/with-consistency :quorum
      (if (seq b)
        (let [b (f/bytes-to-uuid (first b))]
          (storage/dellink cluster a l b))
        (storage/dellink cluster a l)))
    (msg-done)))

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
    (storage/with-consistency :quorum
      (storage/del-tattr cluster k n s))
    (msg-done)))

(defn exec-get-kattr [cluster [k s]]
  (let [k (f/bytes-to-uuid k)
        s (f/bytes-to-str s)]
    (storage/with-consistency :one
      (msg-kattr (storage/get-kattr cluster k s)))))

(defn exec-put-kattr [cluster [k s v]]
  (let [k (f/bytes-to-uuid k)
        s (f/bytes-to-str s)]
    (storage/with-consistency :one
      (storage/put-kattr cluster k s v))
    (msg-done)))

(defn exec-del-kattr [cluster [k s]]
  (let [k (f/bytes-to-uuid k)
        s (f/bytes-to-str s)]
    (storage/with-consistency :quorum
      (storage/del-kattr cluster k s))
    (msg-done)))

(defn exec-getlabel-exact [cluster [k n]]
  (let [k (f/bytes-to-uuid k)
        n (f/bytes-to-str n)]
    (storage/with-consistency :one
      (storage/hasindex cluster k +index-pxlabel+ n))))

(defn exec-getlabel-all [cluster [k page & limit]]
  (let [k (f/bytes-to-uuid k)
        page (f/bytes-to-str page)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/getindex cluster k +index-pxlabel+ page)))))

(defn exec-getlabel-prefix [cluster [k start finish & limit]]
  (let [k (f/bytes-to-uuid k)
        start (f/bytes-to-str start)
        finish (f/bytes-to-str finish)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (storage/getindex cluster k +index-pxlabel+ start finish)))))

(defn exec-getlabel-suffix [cluster [k start finish & limit]]
  (let [k (f/bytes-to-uuid k)
        start (f/bytes-to-str start)
        finish (f/bytes-to-str finish)]
    (storage/with-consistency :one
      (storage/with-limit (f/maybe-bytes-to-str (first limit))
        (map s/reverse (storage/getindex cluster k +index-sxlabel+ (s/reverse start) (s/reverse finish)))))))

(defn exec-getlabel [cluster msg]
  (case (f/bytes-to-str (first msg))
    "all" (msg-label (exec-getlabel-all cluster (drop 1 msg)))
    "pre" (msg-label (exec-getlabel-prefix cluster (drop 1 msg)))
    "suf" (msg-label (exec-getlabel-suffix cluster (drop 1 msg)))
    "ext" (msg-label (exec-getlabel-exact cluster (drop 1 msg)))
    (msg-fail 400)))

(defn exec-putlabel [cluster [k l]]
  (let [k (f/bytes-to-uuid k)
        l (f/bytes-to-str l)]
    (storage/with-consistency :quorum
      (storage/putindex cluster k +index-pxlabel+ l)
      (storage/putindex cluster k +index-sxlabel+ (s/reverse l))
      (msg-done))))

(defn handle-get [cluster msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-getname cluster (drop 1 msg))
    "guid" (exec-getguid cluster (drop 1 msg))
    "link" (exec-getlink cluster (drop 1 msg))
    "label" (exec-getlabel cluster (drop 1 msg))
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
