;; Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.blackbox.blackbox
  (:use     [clojure.tools.cli :only [cli]]
            [clojure.tools.logging :only [warn]])
  (:import  [org.zeromq ZMQ])
  (:require [clojure.string :as string]
            [leela.blackbox.f :as f]
            [leela.blackbox.network.zmqserver :as zserver]
            [leela.blackbox.storage.cassandra :as storage])
  (:gen-class))

(defn maybe-getenv [u]
  (if (.startsWith u "$")
    (System/getenv (subs u 1))
    u))

(defn -main [& args]
  (let [[options args banner] (cli args
                                   ["--help" "this message"
                                    :default false
                                    :flag true]
                                   ["--cassandra" "the cassandra hosts [comma separated] to connect to"
                                    :default ["localhost"]
                                    :parse-fn #(string/split % #",")]
                                   ["--keyspace" "the keyspace to use"
                                    :default "leela"]
                                   ["--username" "the username to use when connecting to cassandra"
                                    :default "$LEELA_CASSANDRA_USERNAME"]
                                   ["--password" "the password to use when connecting to cassandra"
                                    :default "$LEELA_CASSANDRA_PASSWORD"]
                                   ["--capabilities" "the number of workers to fork"
                                    :default 64
                                    :parse-fn #(Integer/parseInt %)]
                                   ["--queue-size" "the number of outstanding requests"
                                    :default 32
                                    :parse-fn #(Integer/parseInt %)]
                                   ["--endpoint" "the binding address"
                                    :default "tcp://localhost:50021"])]
    (when (:help options)
      (println banner)
      (System/exit 0))

    (let [cassandra-args {:credentials {:username (maybe-getenv (:username options))
                                        :password (maybe-getenv (:password options))}
                          :connections (:capabilities options)}
          endpoint       (:endpoint options)]
      (f/supervise
       (storage/with-session [cluster (:cassandra options) (:keyspace options) cassandra-args]
         (zserver/server-start (ZMQ/context 1) cluster (assoc options :endpoint endpoint)))))))
