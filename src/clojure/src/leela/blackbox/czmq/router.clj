(ns leela.blackbox.czmq.router
  (:use     [clojure.tools.logging :only [error info]]
            [leela.blackbox.config :as cfg])
  (:import  [org.zeromq ZMQ ZMQ$Poller])
  (:require [leela.blackbox.f :as f]
            [leela.blackbox.czmq.zhelpers :as z]))

(def s (java.util.concurrent.TimeUnit/SECONDS))

(defn make-queue [size]
  (java.util.concurrent.LinkedBlockingQueue. size))

(defn enqueue [fh queue]
  (let [[peer blank msg] (z/recvmulti fh)]
    (when (empty? blank)
      (.put queue [peer (f/utf8-string msg)]))))

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

(defn run-worker [watch ctx endpoint queue worker]
  (with-open [fh (.socket ctx ZMQ/PUSH)]
    (.connect (z/setup-socket fh) endpoint)
    (f/forever-with
     watch
     (let [item (.poll queue 60 s)]
       (when item (z/sendmulti fh (evaluate worker item)))))))

(defn fork-worker [watch ctx endpoint queue worker]
  (.start (Thread. #(f/supervise-with watch (run-worker watch ctx endpoint queue worker)))))

(defn router-start1 [ctx worker]
  (with-open [ifh (.socket ctx ZMQ/ROUTER)
              ofh (.socket ctx ZMQ/PULL)]
    (let [[cfg watch] (cfg/get-state :router)
          ipcEndpoint (format "inproc://%s.blackbox" (java.util.UUID/randomUUID))
          queue       (make-queue (:queue-size cfg))]
      (.bind (z/setup-socket ofh) ipcEndpoint)
      (.bind (z/setup-socket ifh) (:endpoint cfg))
      (dotimes [_ (:capabilities cfg)] (fork-worker watch ctx ipcEndpoint queue worker))
      (f/supervise-with watch (routing-loop ifh ofh queue)))))

(defmacro router-start [ctx worker]
  `(f/supervise (router-start1 ~ctx ~worker)))
