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

(def nilarr (byte-array 0))

(defn make-queue [size]
  (java.util.concurrent.LinkedBlockingQueue. size))

(defn enqueue [fh queue worker]
  (let [[peer [blank & msg]] (split-with #(not (empty? %)) (z/recvmulti fh))
        time                 (System/currentTimeMillis)]
    (when (and (empty? blank) (not (empty? msg)))
      (when-not (.offer queue [time peer msg])
        (do
          (warn "queue full, dropping request")
          (z/sendmulti fh (concat peer (cons nilarr (:onerr worker)))))))))

(defn routing-loop [ifh ofh queue worker]
  (let [[ifh-info ofh-info] (z/poll -1 [ifh [ZMQ$Poller/POLLIN] ofh [ZMQ$Poller/POLLIN]])]
    (when (.isReadable ifh-info)
      (enqueue ifh queue worker))
    (when (.isReadable ofh-info)
      (z/sendmulti ifh (z/recvmulti ofh)))))

(defn evaluate [worker msg]
  (try
    ((:onjob worker) msg)
    (catch Exception e
      (:onerr worker))))

(defn run-worker [ctx myid endpoint queue worker]
  (with-open [fh (.socket ctx ZMQ/PUSH)]
    (trace (format "ENTER:run-worker %s" endpoint))
    (.connect (z/setup-socket fh) endpoint)
    (f/forever
     (let [[time peer msg] (.take queue)
           reply           (evaluate worker msg)
           req-id          (pr-str (map f/bytes-to-str-unsafe (take 2 msg)))
           rep-id          (pr-str (take 1 reply))]
       (z/sendmulti fh (concat peer (cons nilarr reply)))
       (info (format "WORKER[%04d] %s ~ %s [%d ms]" myid req-id rep-id (- (System/currentTimeMillis) time)))))))

(defn fork-worker [ctx myid endpoint queue worker]
  (.start (Thread. #(f/supervise (run-worker ctx myid endpoint queue worker)))))

(defn router-start1 [ctx worker cfg]
  (trace "ENTER:router-start1")
  (with-open [ifh (.socket ctx ZMQ/ROUTER)
              ofh (.socket ctx ZMQ/PULL)]
    (let [queue       (make-queue (:queue-size cfg))
          ipcendpoint (format "inproc://blackbox-router-%s" (f/uuid-1))]
      (info (format "router-start1; config=%s" cfg))
      (.setSndHWM ofh 0)
      (.setSndHWM ifh 0)
      (.setRcvHWM ofh 0)
      (.setRcvHWM ifh 0)
      (.bind (z/setup-socket ofh) ipcendpoint)
      (.bind (z/setup-socket ifh) (:endpoint cfg))
      (dotimes [myid (:capabilities cfg)] (fork-worker ctx myid ipcendpoint queue worker))
      (f/supervise (routing-loop ifh ofh queue worker))))
  (trace "EXIT:router-start1"))

(defmacro router-start [ctx worker cfg]
  `(f/supervise (router-start1 ~ctx ~worker ~cfg)))
