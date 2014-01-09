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

(defn msg-attr [msg]
  (if-not msg
    (msg-fail 404)
    (cons "attr" msg)))

(defn exec-getname [cluster msg]
  (if (not= (count msg) 1)
    (msg-fail 400)
    (let [g (first msg)]
      (storage/with-consistency :one
        (storage/with-limit 1
          (msg-name (first (map #(f/str-to-json %) (storage/getindex cluster g +index-name+)))))))))

(defn exec-putname [cluster msg]
  (if (not= (count msg) 3)
    (msg-fail 400)
    (let [[g n k] msg]
      (storage/with-consistency :quorum
        (storage/putindex cluster g +index-name+ (f/json-to-str [n k]))
        (msg-done)))))

(defn exec-getlink [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[_ _ limit] msg]
      (storage/with-consistency :one
        (storage/with-limit limit
          (msg-link (apply storage/getlink cluster msg)))))))

(defn exec-putlink [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[a b] msg]
      (storage/with-consistency :quorum
        (storage/putlink cluster a b)
        (msg-done)))))

(defn exec-dellink [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[a b] msg]
      (storage/with-consistency :quorum
        (if (seq b)
          (storage/dellink cluster a b)
          (storage/dellink cluster a)))
      (msg-done))))

(defn exec-getattr [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[k limit] msg]
      (storage/with-consistency :one
        (storage/with-limit limit
          (msg-attr (storage/getattr cluster k)))))))

(defn exec-putattr [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[k t v] msg]
      (storage/with-consistency :quorum
        (storage/putattr cluster k t v))
      (msg-done))))

(defn exec-delattr [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[k] msg]
      (storage/with-consistency :quorum
        (storage/delattr cluster k))
      (msg-done))))

(defn exec-getlabel-exact [cluster msg]
  (if (not= (count msg) 2)
    (msg-fail 400)
    (let [[k n] msg]
      (storage/with-consistency :one
        (storage/hasindex cluster k +index-pxlabel+ n)))))

(defn exec-getlabel-all [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[k page limit] msg]
      (storage/with-consistency :one
        (storage/with-limit limit
          (storage/getindex cluster k +index-pxlabel+ page))))))

(defn exec-getlabel-prefix [cluster msg]
  (if (< (count msg) 3)
    (msg-fail 400)
    (let [[k start finish limit] msg]
      (storage/with-consistency :one
        (storage/with-limit limit
          (storage/getindex cluster k +index-pxlabel+ start finish))))))

(defn exec-getlabel-suffix [cluster msg]
  (if (not= (count msg) 3)
    (msg-fail 400)
    (let [[k start finish limit] msg]
      (storage/with-consistency :one
        (storage/with-limit limit
          (map s/reverse (storage/getindex cluster k +index-sxlabel+ (s/reverse start) (s/reverse finish))))))))

(defn exec-getlabel [cluster msg]
  (case (first msg)
    "all" (msg-label (exec-getlabel-all cluster (subvec msg 1)))
    "pre" (msg-label (exec-getlabel-prefix cluster (subvec msg 1)))
    "suf" (msg-label (exec-getlabel-suffix cluster (subvec msg 1)))
    "ext" (msg-label (exec-getlabel-exact cluster (subvec msg 1)))
    (msg-fail 400)))

(defn exec-putlabel [cluster msg]
  (if (< (count msg) 1)
    (msg-fail 400)
    (let [[k l] msg]
      (storage/with-consistency :quorum
          (storage/putindex cluster k +index-pxlabel+ l)
          (storage/putindex cluster k +index-sxlabel+ (s/reverse l))
        (msg-done)))))

(defn handle-get [cluster msg]
  (case (first msg)
    "name" (exec-getname cluster (subvec msg 1))
    "link" (exec-getlink cluster (subvec msg 1))
    "label" (exec-getlabel cluster (subvec msg 1))
    "attr" (exec-getattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-put [cluster msg]
  (case (first msg)
    "name" (exec-putname cluster (subvec msg 1))
    "link" (exec-putlink cluster (subvec msg 1))
    "label" (exec-putlabel cluster (subvec msg 1))
    "attr" (exec-putattr cluster (subvec msg 1))
    (msg-fail 400)))

(defn handle-del [cluster msg]
  (case (first msg)
    "link" (exec-dellink cluster (subvec msg 1))
    "attr" (exec-delattr cluster (subvec msg 1))
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
