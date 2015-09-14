;; Copyright (c) 2015 <Diego Souza>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns leela.storaged.network.router
  (:import
   [org.zeromq ZMQ ZMQ$Socket ZMQ$Context ZMQ$Poller ZMQ$PollItem]
   [java.util.concurrent TimeUnit BlockingQueue LinkedBlockingQueue])
  (:require
   [clojure.tools.logging :refer [trace info warn]]
   [leela.storaged.control :refer [forever supervise]]
   [leela.storaged.network.zhelpers :as z]
   [leela.storaged.network.protocol :refer [unframe]]
   [leela.storaged.network.controller :as ctrl]))

(defn- enqueue [fh ^BlockingQueue queue exec-fn [peer body]]
  (when-not (or (empty? peer) (empty? body))
    (when-not (.offer queue [peer body])
      (do
        (warn "queue full, dropping request")
        (z/sendmsg fh peer ((:onerr exec-fn)))))))

(defn- routing-loop [ifh ofh queue exec-fn]
  (forever
   (let [[^ZMQ$PollItem ifh-info ^ZMQ$PollItem ofh-info] (z/poll -1 [ifh [ZMQ$Poller/POLLIN] ofh [ZMQ$Poller/POLLIN]])]
     (when (.isReadable ifh-info)
       (enqueue ifh queue exec-fn (z/recvmsg ifh)))
     (when (.isReadable ofh-info)
       (let [[peer msg] (z/recvmsg ofh)]
         (z/sendmsg ifh peer msg))))))

(defn- run-worker [^ZMQ$Context ctx tid endpoint ^BlockingQueue queue exec-fn]
  (with-open [fh (.socket ctx ZMQ/PUSH)]
    (trace (format "ENTER:run-worker %d:%s" tid endpoint))
    (.connect (z/setup-socket fh) endpoint)
    (forever
     (let [[peer msg] (.take queue)
           reply      ((:onjob exec-fn) (first msg))]
       (z/sendmsg fh peer [reply])))))

(defn- fork-worker [ctx tid endpoint queue exec-fn]
  (.start (Thread. #(supervise (run-worker ctx tid endpoint queue exec-fn)))))

(defn- router-start1 [^ZMQ$Context ctx exec-fn cfg]
  (trace "ENTER:router-start1")
  (with-open [ifh (.socket ctx ZMQ/ROUTER)
              ofh (.socket ctx ZMQ/PULL)]
    (let [queue    (LinkedBlockingQueue. ^int (:queue-size cfg))
          endpoint "inproc://storaged.pipe"]
      (info (format "router-start1; config=%s" cfg))
      (.bind (z/setup-socket ofh) endpoint)
      (.bind (z/setup-socket ifh) (:endpoint cfg))
      (dotimes [tid (:capabilities cfg)] (fork-worker ctx tid endpoint queue exec-fn))
      (supervise (routing-loop ifh ofh queue exec-fn))))
  (trace "EXIT:router-start1"))

(defn router-start [ctx exec-fn cfg]
  (supervise (router-start1 ctx exec-fn cfg)))
