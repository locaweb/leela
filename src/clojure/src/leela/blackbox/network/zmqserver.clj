(ns leela.blackbox.network.zmqserver
  (:use     [clojure.tools.logging :only [info error]])
  (:require [leela.blackbox.f :as f]
            [clojure.data.json :as json]
            [leela.blackbox.czmq.router :as router]
            [leela.blackbox.storage.cassandra :as storage]))

(defn msg-fail [status]
  {:code 4 :data status})

(defn msg-done []
  {:code 0})

(defn msg-name [n k]
  {:code 1 :data [n k]})

(defn msg-node [links]
  {:code 2 :data links})

(defn msg-label [labels]
  {:code 3 :data labels})

(defn exec-getname [session a]
  (storage/with-consistency :quorum
    (let [raw (storage/getname session a)]
      (case raw
        nil (msg-fail 404)
        (apply msg-name (json/read-str raw))))))

(defn exec-putname [session [n k g]]
  (storage/with-consistency :quorum
    (storage/putname session (json/write-str [n k]) g)
    (msg-done)))

(defn exec-getlink [session a]
  (storage/with-consistency :one
    (let [rows (storage/getlink session a)]
      (case (seq rows)
        nil (msg-fail 404)
        (msg-node rows)))))

(defn exec-putlink [session [a links]]
  (storage/with-consistency :quorum
    (storage/putlink session a links)
    (msg-done)))

(defn exec-getlabel [session a]
  (storage/with-consistency :one
    (let [rows (storage/getlabel session a)]
      (case (seq rows)
        nil (msg-fail 404)
        (msg-label rows)))))

(defn exec-putlabel [session [a labels]]
  (storage/with-consistency :quorum
    (storage/putlabel session a labels)
    (msg-done)))

(defn handle-message [session msg]
  (case (get msg "code")
    0 (exec-getname session (get msg "data"))
    1 (exec-putname session (get msg "data"))
    2 (exec-getlabel session (get msg "data"))
    3 (exec-putlabel session (get msg "data"))
    4 (exec-getlink session (get msg "data"))
    5 (exec-putlink session (get msg "data"))
    ; 6 (exec-unlink session (get msg "data"))
    (msg-fail 400)))

(defn my-worker [session]
  {:onjob #(json/write-str (handle-message session (json/read-str %))) :onerr (json/write-str (msg-fail 500))})

(defn server-start [ctx cluster]
  (router/router-start ctx (my-worker cluster)))
  
