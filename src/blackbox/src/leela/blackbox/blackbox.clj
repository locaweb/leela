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

(ns leela.blackbox.blackbox
  (:use     [clojure.tools.cli :only [cli]]
            [clojure.tools.logging :only [warn]])
  (:import  [org.zeromq ZMQ])
  (:require [clojure.string :as string]
            [leela.blackbox.f :as f]
            [leela.blackbox.network.zmqserver :as zserver]
            [leela.blackbox.storage.cassandra :as storage]
            [leela.blackbox.storage.s3 :as s3]
  )
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
                                   ["--attr-keyspace" "the [attr] keyspace to use"
                                    :default "leela"]
                                   ["--graph-keyspace" "the [graph] keyspace to use"
                                    :default "graph"]
                                   ["--username" "the username to use when connecting to cassandra"
                                    :default "$LEELA_CASSANDRA_USERNAME"]
                                   ["--password" "the password to use when connecting to cassandra"
                                    :default "$LEELA_CASSANDRA_PASSWORD"]
                                   ["--s3url" "the url to use when connecting to s3"
                                    :default "$S3_URL"]
                                   ["--s3accesskey" "the accesskey to use when connecting to s3"
                                    :default "$S3_ACCESS_KEY"]
                                   ["--s3secretkey" "the secretkey to use when connecting to s3"
                                    :default "$S3_SECRET_KEY"]
                                   ["--capabilities" "the number of workers to fork"
                                    :default 256
                                    :parse-fn #(Integer/parseInt %)]
                                   ["--queue-size" "the number of outstanding requests"
                                    :default 128
                                    :parse-fn #(Integer/parseInt %)]
                                   ["--endpoint" "the binding address"
                                    :default "tcp://*:50021"])]
    (when (:help options)
      (println banner)
      (System/exit 0))

    (let [cassandra-args {:credentials {:username (maybe-getenv (:username options))
                                        :password (maybe-getenv (:password options))}
                          :max-connections 64
                          :connections 1}
          endpoint       (:endpoint options)]
    (let [s3cred         {:access-key (maybe-getenv (:s3accesskey options))
                          :secret-key (maybe-getenv (:s3secretkey options))
                          :endpoint (maybe-getenv (:s3url options))}]
      (f/supervise
       (storage/with-connection [attr-cluster (:cassandra options) cassandra-args]
         (storage/with-connection [graph-cluster (:cassandra options) cassandra-args]
           (storage/use-attr-schema attr-cluster (:attr-keyspace options))
           (storage/use-graph-schema graph-cluster (:graph-keyspace options))
           (zserver/server-start (ZMQ/context 1) attr-cluster graph-cluster (assoc options :endpoint endpoint)))))))))
