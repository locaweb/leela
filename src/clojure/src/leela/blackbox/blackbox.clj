(ns leela.blackbox.blackbox
  (:use     [clojure.tools.cli :only [cli]]
            [leela.blackbox.config :as cfg])
  (:import  [org.zeromq ZMQ])
  (:require [leela.blackbox.config :as config]
            [leela.blackbox.network.zmqserver :as zserver]
            [leela.blackbox.storage.cassandra :as storage])
  (:gen-class))

(defn maybe-cat [default f]
  (if (and f (.canRead (java.io.File. f)))
    (first (clojure.string/split (slurp f) #"\n"))
    default))

(defn -main [& args]
  (let [[options args banner] (cli args
                                   ["--help" "this message"
                                    :default false
                                    :flag true]
                                   ["--zookeeper" "the zookeeper address to connect to"
                                    :default "127.0.0.1:2181/leela"]
                                   ["--zk-authfile" "the file to read the credentials from"]
                                   ["--cassandra" "the cassandra cluster to connect to [comma separated]"
                                    :default "127.0.0.1"]
                                   ["--keyspace" "the keyspace to use"
                                    :default "leela"]
                                   ["--node" "the nodename to report and also to read config from"
                                    :default (.. java.net.InetAddress getLocalHost getHostName)])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (config/zk-attach (:node options) (:zookeeper options) (maybe-cat "leela:leela" (:zk-authfile options)))
    (storage/with-session [cluster (:cassandra options) (:keyspace options)]
      (zserver/server-start (ZMQ/context 1) cluster))))
