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

(defn msg-name [msg]
  (if-not msg
    (msg-fail 404)
    (cons "name" msg)))

(defn msg-link [links]
  (cons "link" (map f/binary-to-hexstr links)))

(defn msg-label [labels]
  (cons "label" labels))

(defn msg-tattr [msg]
  (cons "t-attr" (flatten (map (fn [[k v]] [(str k) (f/binary-to-hexstr v)]) msg))))

(defn msg-kattr [data]
  (cons "k-attr" (if data (f/binary-to-hexstr data) "")))

(defn exec-getname [cluster [g]]
  (let [g (f/hexstr-to-binary g)]
    (storage/with-consistency :one
      (storage/with-limit 1
        (msg-name (first (map #(f/str-to-json %) (storage/getindex cluster g +index-name+))))))))

(defn exec-putname [cluster [g n k]]
  (let [g (f/hexstr-to-binary g)]
    (storage/with-consistency :quorum
      (storage/putindex cluster g +index-name+ (f/json-to-str [n k]))
      (msg-done))))

(defn exec-getlink [cluster [a l page & limit]]
  (let [a (f/bytes-to-binary a)
        page (f/bytes-to-binary page)]
    (storage/with-consistency :one
      (storage/with-limit (first limit)
        (msg-link (storage/getlink cluster a l page))))))

(defn exec-putlink [cluster [a l b]]
  (let [a (f/bytes-to-binary a)
        b (f/bytes-to-binary b)]
    (storage/with-consistency :quorum
      (storage/putlink cluster a l b)
      (msg-done))))

(defn exec-dellink [cluster [a l & b]]
  (let [a (f/bytes-to-binary a)]
    (storage/with-consistency :quorum
      (if (seq b)
        (let [b (f/bytes-to-binary (first b))]
          (storage/dellink cluster a l b))
        (storage/dellink cluster a l)))
    (msg-done)))

(defn exec-get-tattr [cluster [k]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :one
      (msg-tattr (storage/get-tattr cluster k)))))

(defn exec-put-tattr [cluster [k s v]]
  (let [k (f/bytes-to-binary k)
        v (f/bytes-to-binary v)]
    (storage/with-consistency :one
      (storage/put-tattr cluster k (Integer. s) v))
    (msg-done)))

(defn exec-del-tattr [cluster [k s]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :quorum
      (storage/del-tattr cluster k (Integer. s)))
    (msg-done)))

(defn exec-get-kattr [cluster [k s]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :one
      (msg-kattr (storage/get-kattr cluster k s)))))

(defn exec-put-kattr [cluster [k s v]]
  (let [k (f/bytes-to-binary k)
        v (f/bytes-to-binary v)]
    (storage/with-consistency :one
      (storage/put-kattr cluster k s v))
    (msg-done)))

(defn exec-del-kattr [cluster [k s]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :quorum
      (storage/del-kattr cluster k s))
    (msg-done)))

(defn exec-getlabel-exact [cluster [k n]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :one
      (storage/hasindex cluster k +index-pxlabel+ n))))

(defn exec-getlabel-all [cluster [k page & limit]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :one
      (storage/with-limit (first limit)
        (storage/getindex cluster k +index-pxlabel+ page)))))

(defn exec-getlabel-prefix [cluster [k start finish & limit]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :one
      (storage/with-limit (first limit)
        (storage/getindex cluster k +index-pxlabel+ start finish)))))

(defn exec-getlabel-suffix [cluster [k start finish & limit]]
  (let [k (f/bytes-to-binary k)]
    (storage/with-consistency :one
      (storage/with-limit (first limit)
        (map s/reverse (storage/getindex cluster k +index-sxlabel+ (s/reverse start) (s/reverse finish)))))))

(defn exec-getlabel [cluster msg]
  (case (f/bytes-to-str (first msg))
    "all" (msg-label (exec-getlabel-all cluster (subvec msg 1)))
    "pre" (msg-label (exec-getlabel-prefix cluster (subvec msg 1)))
    "suf" (msg-label (exec-getlabel-suffix cluster (subvec msg 1)))
    "ext" (msg-label (exec-getlabel-exact cluster (subvec msg 1)))
    (msg-fail 400)))

(defn exec-putlabel [cluster [k l]]
  (let [k (f/hexstr-to-binary k)]
    (storage/with-consistency :quorum
      (storage/putindex cluster k +index-pxlabel+ l)
      (storage/putindex cluster k +index-sxlabel+ (s/reverse l))
      (msg-done))))

(defn handle-get [cluster msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-getname cluster (subvec msg 1))
    "link" (exec-getlink cluster (subvec msg 1))
    "label" (exec-getlabel cluster (subvec msg 1))
    "t-attr" (exec-get-tattr cluster (subvec msg 1))
    "k-attr" (exec-get-kattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-put [cluster msg]
  (case (f/bytes-to-str (first msg))
    "name" (exec-putname cluster (subvec msg 1))
    "link" (exec-putlink cluster (subvec msg 1))
    "label" (exec-putlabel cluster (subvec msg 1))
    "t-attr" (exec-put-tattr cluster (subvec msg 1))
    "k-attr" (exec-put-kattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-del [cluster msg]
  (case (f/bytes-to-str (first msg))
    "link" (exec-dellink cluster (subvec msg 1))
    "t-attr" (exec-del-tattr cluster (subvec msg 1))
    "k-attr" (exec-del-kattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-message [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (case (f/bytes-to-str (first msg))
      "get" (handle-get cluster (subvec msg 1))
      "put" (handle-put cluster (subvec msg 1))
      "del" (handle-del cluster (subvec msg 1))
      (msg-fail 400))))

(defn zmqworker [cluster]
  {:onjob #(handle-message cluster %) :onerr (msg-fail 500)})

(defn server-start [ctx cluster options]
  (router/router-start ctx (zmqworker cluster) options))
