(ns leela.blackbox.network.zmqserver
  (:use     [clojure.tools.logging :only [info error]])
  (:require [clojure.data.json :as json]
            [leela.blackbox.czmq.router :as router]
            [leela.blackbox.storage.cassandra :as storage]))

(defn msg-fail [status]
  {:code 3 :data status})

(defn msg-done []
  {:code 0})

(defn msg-name [n k]
  {:code 1 :data [n k]})

(defn msg-node [links]
  {:code 2 :data links})

(defn exec-resolve [session a]
  (storage/with-consistency :quorum
    (apply msg-name (storage/get-name a))))

(defn exec-putnode [session [n k g]]
  (storage/with-consistency :quorum
    (storage/put-node session n k g)
    (msg-done)))

(defn exec-getnode [session a]
  (storage/with-consistency :one
    (let [rows (storage/get-node session a)]
      (case (seq rows)
        nil (msg-fail 404)
        (msg-node rows)))))

(defn exec-putlink [session links]
  (storage/with-consistency :one
    (storage/put-link session links)
    (msg-done)))

(defn handle-message [session msg]
  (case (get msg "code")
    0 (exec-resolve session (get msg "data"))
    1 (exec-getnode session (get msg "data"))
    2 (exec-putnode session (get msg "data"))
    3 (exec-putlink session (get msg "data"))
    (msg-fail 400)))

(defn my-worker [session]
  {:onjob #(json/write-str (handle-message session (json/read-str %))) :onerr (json/write-str (msg-fail 500))})

(defn create-session [& args]
  (apply storage/session args))

(defn server-start [session ctx cfg]
  (router/router-start ctx cfg (my-worker session)))
  
