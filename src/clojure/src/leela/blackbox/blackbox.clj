(ns leela.blackbox.blackbox
  (:use     [clojure.tools.cli :only [cli]]
            [leela.blackbox.config :as cfg])
  (:import  [org.zeromq ZMQ])
  (:require [leela.blackbox.network.zmqserver :as zserver]
            [leela.blackbox.storage.cassandra :as storage])
  (:gen-class))

(defn seed-assoc-fn [opts k v]
  (assoc opts k
         (case k
           :seed (if-let [oldval (get opts k)]
                   (conj oldval v)
                   [v])
           v)))

(defn -main [& args]
  (let [[options args banner] (cli args
                                   ["--help" "this message"
                                    :default false
                                    :flag true]
                                   ["--zookeeper" "the zookeeper address to connect to"
                                    :default "tcp://localhost:2181"]
                                   ["--namespace" "our namespace"
                                    :default "leela-devel"]
                                   ["--node" "the nodename to report and also to read config from"
                                    :default (.. java.net.InetAddress getLocalHost getHostName)])]
    (when (:help options)
      (println banner)
      (java.lang.System/exit 0))
    (storage/with-session [cluster]
      (zserver/server-start (ZMQ/context 1) cluster))))
