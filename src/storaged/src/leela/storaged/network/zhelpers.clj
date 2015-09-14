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

(ns leela.storaged.network.zhelpers
  (:import
   [org.zeromq ZMQ ZMQ$Poller ZMQ$Socket ZMQ$PollItem]
   [java.util.concurrent.TimeUnit]
   [java.util.concurrent.ArrayBlockingQueue])
  (:require
   [clojure.tools.logging :refer [info warn]]))

(def empty-msg (byte-array 0))

(defn- run-recvmsg [state msg]
  (if-let [acc (:body state)]
    (assoc state :body (conj acc msg))
    (if (or (nil? msg) (empty? msg))
      (assoc state :body [])
      (if-let [acc (:peer state)]
        (assoc state :peer (conj acc msg))
        (assoc state :peer [msg])))))

(defn- eval-recvmsg [state]
  (if-let [body (:body state)]
    (list (:peer state) body)
    (list [] (:peer state))))

(defn- make-pollitem [^ZMQ$Socket fh events]
  (let [^long flags (apply bit-or (conj events 0))]
    (ZMQ$PollItem. fh flags)))

(defn- make-pollitems [watch]
  (let [events (map #(apply make-pollitem %) (partition 2 watch))]
    (into-array ZMQ$PollItem events)))

(defn setup-socket ^ZMQ$Socket [^ZMQ$Socket fh]
  (.setLinger fh 0)
  (.setReconnectIVL fh 1000)
  (.setMaxMsgSize fh (* 10 1024 1024))
  fh)

(defn recvmsg [^ZMQ$Socket fh]
  (loop [state0 {}]
    (let [state1 (run-recvmsg state0 (.recv fh))]
      (if (.hasReceiveMore fh)
        (recur state1)
        (eval-recvmsg state1)))))

(defn poll [timeout watch]
  (let [pollitems (make-pollitems watch)]
    (ZMQ/poll pollitems timeout)
    pollitems))

(defn- socket-send
  ([^ZMQ$Socket fh ^bytes msg]
   (.send fh msg 0))
  ([^ZMQ$Socket fh ^bytes msg ^long flags]
   (.send fh msg flags)))

(defn sendmsg [fh peer body]
  (doseq [msg peer]
    (socket-send fh msg ZMQ/SNDMORE))
  (socket-send fh empty-msg ZMQ/SNDMORE)
  (loop [[x & xs] body]
    (if (empty? xs)
      (do
        (socket-send fh x))
      (do
        (socket-send fh x ZMQ/SNDMORE)
        (recur xs)))))
