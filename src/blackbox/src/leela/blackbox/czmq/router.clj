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

(ns leela.blackbox.czmq.router
  (:use     [clojure.tools.logging :only [trace debug error info warn]])
  (:import  [org.zeromq ZMQ ZMQ$Poller])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.czmq.zhelpers :as z]))

(def s java.util.concurrent.TimeUnit/SECONDS)

(defn make-queue [size]
  (java.util.concurrent.LinkedBlockingQueue. size))

(defn enqueue [fh queue]
  (let [[peer [blank & msg]] (split-with #(not (empty? %)) (z/recvmulti fh))]
    (when (empty? blank)
      (.put queue [peer msg]))))

(defn routing-loop [ifh ofh queue]
  (let [[ifh-info ofh-info] (z/poll -1 [ifh [ZMQ$Poller/POLLIN] ofh [ZMQ$Poller/POLLIN]])]
    (when (.isReadable ifh-info)
      (enqueue ifh queue))
    (when (.isReadable ofh-info)
      (z/sendmulti ifh (z/recvmulti ofh)))))

(defn evaluate [worker [peer msg]]
  (try
    (let [reply ((:onjob worker) msg)]
      (debug (format "REQUEST/DONE: %s ~> %s" (pr-str (map f/bytes-to-str-unsafe msg)) (pr-str reply)))
      (concat peer (cons "" reply)))
    (catch Exception e
      (let [reply (:onerr worker)]
        (error e (format "REQUEST/FAIL: %s ~> %s" (pr-str (map f/bytes-to-str-unsafe msg)) (pr-str reply)))
        (concat peer (cons "" reply))))))

(defn run-worker [ctx endpoint queue worker]
  (with-open [fh (.socket ctx ZMQ/PUSH)]
    (trace (format "ENTER:run-worker %s" endpoint))
    (.connect (z/setup-socket fh) endpoint)
    (f/forever
     (let [item (.poll queue 1 s)]
       (when item (z/sendmulti fh (evaluate worker item)))))))

(defn fork-worker [ctx endpoint queue worker]
  (.start (Thread. #(f/supervise (run-worker ctx endpoint queue worker)))))

(defn router-start1 [ctx worker cfg]
  (trace "ENTER:router-start1")
  (with-open [ifh (.socket ctx ZMQ/ROUTER)
              ofh (.socket ctx ZMQ/PULL)]
    (let [queue       (make-queue (:queue-size cfg))
          ipcendpoint "inproc://blackbox.router"]
      (info (format "router-start1; config=%s" cfg))
      (.bind (z/setup-socket ofh) ipcendpoint)
      (.bind (z/setup-socket ifh) (:endpoint cfg))
      (dotimes [_ (:capabilities cfg)] (fork-worker ctx ipcendpoint queue worker))
      (f/supervise (routing-loop ifh ofh queue))))
  (trace "EXIT:router-start1"))

(defmacro router-start [ctx worker cfg]
  `(f/supervise (router-start1 ~ctx ~worker ~cfg)))
