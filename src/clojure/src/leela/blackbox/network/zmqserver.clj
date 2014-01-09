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
  (cons "link" links))

(defn msg-label [labels]
  (cons "label" labels))

(defn msg-tattr [msg]
  (if-not msg
    (msg-fail 404)
    (cons "tattr" msg)))

(defn msg-kattr [msg]
  (if-not msg
    (msg-fail 404)
    (cons "kattr" msg)))

(defn exec-getname [cluster [g]]
  (storage/with-consistency :one
    (storage/with-limit 1
      (msg-name (first (map #(f/str-to-json %) (storage/getindex cluster g +index-name+)))))))

(defn exec-putname [cluster [g n k]]
  (storage/with-consistency :quorum
    (storage/putindex cluster g +index-name+ (f/json-to-str [n k]))
    (msg-done)))

(defn exec-getlink [cluster [a l page & limit]]
  (storage/with-consistency :one
    (storage/with-limit (first limit)
      (msg-link (storage/getlink cluster a l page)))))

(defn exec-putlink [cluster [a l b]]
  (storage/with-consistency :quorum
    (storage/putlink cluster a l b)
    (msg-done)))

(defn exec-dellink [cluster [a l & b]]
  (storage/with-consistency :quorum
    (if (seq b)
      (storage/dellink cluster a l (first b))
      (storage/dellink cluster a l)))
  (msg-done))

(defn exec-gettattr [cluster [k]]
  (storage/with-consistency :one
    (storage/with-limit 1
      (msg-tattr (storage/gettattr cluster k)))))

(defn exec-puttattr [cluster [k s t]]
  (storage/with-consistency :one
    (storage/puttattr cluster k (Integer. s) t))
  (msg-done))

(defn exec-deltattr [cluster [k s]]
  (storage/with-consistency :quorum
    (storage/deltattr cluster k (Integer. s)))
  (msg-done))

(defn exec-getkattr [cluster [k s]]
  (storage/with-consistency :one
    (storage/with-limit 1
      (msg-kattr (storage/getkattr cluster k s)))))

(defn exec-putkattr [cluster [k s v]]
  (storage/with-consistency :one
    (storage/putkattr cluster k s v))
  (msg-done))

(defn exec-delkattr [cluster [k s]]
  (storage/with-consistency :quorum
    (storage/delkattr cluster k s))
  (msg-done))

(defn exec-getlabel-exact [cluster [k n]]
  (storage/with-consistency :one
    (storage/hasindex cluster k +index-pxlabel+ n)))

(defn exec-getlabel-all [cluster [k page & limit]]
  (storage/with-consistency :one
    (storage/with-limit (first limit)
      (storage/getindex cluster k +index-pxlabel+ page))))

(defn exec-getlabel-prefix [cluster [k start finish & limit]]
  (storage/with-consistency :one
    (storage/with-limit (first limit)
      (storage/getindex cluster k +index-pxlabel+ start finish))))

(defn exec-getlabel-suffix [cluster [k start finish & limit]]
  (storage/with-consistency :one
    (storage/with-limit (first limit)
      (map s/reverse (storage/getindex cluster k +index-sxlabel+ (s/reverse start) (s/reverse finish))))))

(defn exec-getlabel [cluster msg]
  (case (first msg)
    "all" (msg-label (exec-getlabel-all cluster (subvec msg 1)))
    "pre" (msg-label (exec-getlabel-prefix cluster (subvec msg 1)))
    "suf" (msg-label (exec-getlabel-suffix cluster (subvec msg 1)))
    "ext" (msg-label (exec-getlabel-exact cluster (subvec msg 1)))
    (msg-fail 400)))

(defn exec-putlabel [cluster [k l]]
  (storage/with-consistency :quorum
    (storage/putindex cluster k +index-pxlabel+ l)
    (storage/putindex cluster k +index-sxlabel+ (s/reverse l))
    (msg-done)))

(defn handle-get [cluster msg]
  (case (first msg)
    "name" (exec-getname cluster (subvec msg 1))
    "link" (exec-getlink cluster (subvec msg 1))
    "label" (exec-getlabel cluster (subvec msg 1))
    "tattr" (exec-gettattr cluster (subvec msg 1))
    "kattr" (exec-getkattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-put [cluster msg]
  (case (first msg)
    "name" (exec-putname cluster (subvec msg 1))
    "link" (exec-putlink cluster (subvec msg 1))
    "label" (exec-putlabel cluster (subvec msg 1))
    "tattr" (exec-puttattr cluster (subvec msg 1))
    "kattr" (exec-putkattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-del [cluster msg]
  (case (first msg)
    "link" (exec-dellink cluster (subvec msg 1))
    "tattr" (exec-deltattr cluster (subvec msg 1))
    "kattr" (exec-delkattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-message [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [msg (mapv f/bytes-to-str msg)]
      (case (first msg)
        "get" (handle-get cluster (subvec msg 1))
        "put" (handle-put cluster (subvec msg 1))
        "del" (handle-del cluster (subvec msg 1))
        (msg-fail 400)))))

(defn zmqworker [cluster]
  {:onjob #(handle-message cluster %) :onerr (msg-fail 500)})

(defn server-start [ctx cluster options]
  (router/router-start ctx (zmqworker cluster) options))
