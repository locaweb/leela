(ns leela.blackbox.czmq.router
  (:use     [clojure.tools.logging :only [error]])
  (:import  [org.zeromq ZMQ ZMQ$Poller])
  (:require [leela.blackbox.czmq.zhelpers :as z]))

(defn make-queue [size]
  (java.util.concurrent.LinkedBlockingQueue. size))

(defn enqueue [fh queue]
  (let [[peer blank msg] (z/recvmulti fh)]
    (when (empty? blank)
      (.put queue [peer (z/utf8-string msg)]))))

(defn routing-loop [ifh ofh queue]
  (let [[ifh-info ofh-info] (z/poll -1 [ifh [ZMQ$Poller/POLLIN] ofh [ZMQ$Poller/POLLIN]])]
    (when (.isReadable ifh-info)
      (enqueue ifh queue))
    (when (.isReadable ofh-info)
      (z/sendmulti ifh (z/recvmulti ofh)))))

(defn evaluate [worker [peer msg]]
  (try
    [peer "" ((:onjob worker) msg)]
    (catch Exception e
      (error e "error evaluating worker")
      [peer "" (:onerr worker)])))

(defn run-worker [ctx endpoint queue worker]
  (with-open [fh (.socket ctx ZMQ/PUSH)]
    (.connect (z/setup-socket fh) endpoint)
    (z/forever (z/sendmulti fh (evaluate worker (.take queue))))))

(defn fork-worker [ctx endpoint queue worker]
  (.start (Thread. #(z/supervise (run-worker ctx endpoint queue worker)))))

(defn router-start [ctx cfg worker]
  (with-open [ifh (.socket ctx ZMQ/ROUTER)
              ofh (.socket ctx ZMQ/PULL)]
    (let [ipcEndpoint (format "inproc:///tmp/%s.blackbox" (java.util.UUID/randomUUID))
          queue       (make-queue (:queue-size cfg))]
      (.bind (z/setup-socket ofh) ipcEndpoint)
      (.bind (z/setup-socket ifh) (:endpoint cfg))
      (dotimes [_ (:capabilities cfg)] (fork-worker ctx ipcEndpoint queue worker))
      (z/forever (routing-loop ifh ofh queue)))))
