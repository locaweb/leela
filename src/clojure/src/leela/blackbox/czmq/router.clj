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

(ns leela.blackbox.czmq.router
  (:use     [clojure.tools.logging :only [trace debug error info warn]]
            [leela.blackbox.config :as cfg])
  (:import  [org.zeromq ZMQ ZMQ$Poller])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.czmq.zhelpers :as z]))

(def s java.util.concurrent.TimeUnit/SECONDS)

(defn make-queue [size]
  (java.util.concurrent.LinkedBlockingQueue. size))

(defn enqueue [fh queue]
  (let [[peer blank & msg] (z/recvmulti fh)]
    (when (empty? blank)
      (.put queue [peer msg]))))

(defn routing-loop [ifh ofh queue]
  (trace "ENTER:routing-loop")
  (let [[ifh-info ofh-info] (z/poll 1000 [ifh [ZMQ$Poller/POLLIN] ofh [ZMQ$Poller/POLLIN]])]
    (when (.isReadable ifh-info)
      (enqueue ifh queue))
    (when (.isReadable ofh-info)
      (z/sendmulti ifh (z/recvmulti ofh)))))

(defn evaluate [worker [peer msg]]
  (try
    (cons peer (cons "" ((:onjob worker) msg)))
    (catch Exception e
      (error e "error evaluating worker")
      (cons peer (cons "" (:onerr worker))))))

(defn run-worker [watch ctx endpoint queue worker]
  (with-open [fh (.socket ctx ZMQ/PUSH)]
    (trace (format "ENTER:run-worker %s" endpoint))
    (.connect (z/setup-socket fh) endpoint)
    (f/forever-with
     watch
     (let [item (.poll queue 1 s)]
       (when item (z/sendmulti fh (evaluate worker item)))))))

(defn fork-worker [watch ctx endpoint queue worker]
  (.start (Thread. #(f/supervise-with watch (run-worker watch ctx endpoint queue worker)))))

(defn router-start1 [ctx worker]
  (trace "ENTER:router-start1")
  (with-open [ifh (.socket ctx ZMQ/ROUTER)
              ofh (.socket ctx ZMQ/PULL)]
    (let [[cfg watch] (cfg/get-state :zmqrouter)
          queue       (make-queue (:queue-size cfg))
          ipcendpoint (format "inproc://blackbox.v%d" (:version cfg))]
      (info (format "router-start1; config=%s" cfg))
      (.bind (z/setup-socket ofh) ipcendpoint)
      (.bind (z/setup-socket ifh) (:endpoint cfg))
      (dotimes [_ (:capabilities cfg)] (fork-worker watch ctx ipcendpoint queue worker))
      (f/supervise-with watch (routing-loop ifh ofh queue))))
  (trace "EXIT:router-start1"))

(defmacro router-start [ctx worker]
  `(f/supervise (router-start1 ~ctx ~worker)))
