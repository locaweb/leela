;; This file is part of Leela.
;;
;; Leela is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Leela is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Leela.  If not, see <http://www.gnu.org/licenses/>.

(ns leela.blackbox.network.zmqserver
  (:use     [clojure.tools.logging :only [info error]])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.czmq.router :as router]
            [leela.blackbox.storage.cassandra :as storage]))

(defn msg-fail [status]
  {:code 4 :data status})

(defn msg-done []
  {:code 0})

(defn msg-name [n k]
  {:code 1 :data [n k]})

(defn msg-link [links]
  {:code 2 :data links})

(defn msg-label [labels]
  {:code 3 :data labels})

(defn exec-getname [session a]
  (storage/with-consistency :quorum
    (let [raw (storage/getname session a)]
      (case raw
        nil (msg-fail 404)
        (apply msg-name (f/str-to-json raw))))))

(defn exec-putname [session [n k g]]
  (storage/with-consistency :quorum
    (storage/putname session (f/json-to-str [n k]) g)
    (msg-done)))

(defn exec-getlink [session a]
  (storage/with-consistency :one
    (let [rows (storage/getlink session a)]
      (case (seq rows)
        nil (msg-fail 404)
        (msg-link rows)))))

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
    (msg-fail 400)))

(defn zmqworker [cluster]
  {:onjob #(f/json-to-str (handle-message cluster (f/str-to-json %))) :onerr (f/json-to-str (msg-fail 500))})

(defn server-start [ctx cluster]
  (router/router-start ctx (zmqworker cluster)))
  
