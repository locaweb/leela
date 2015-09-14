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

(ns leela.storaged.main
  (:import
   [org.zeromq ZMQ])
  (:require
   [clojure.string :refer [split]]
   [clojure.tools.cli :refer [cli]]
   [leela.storaged.control :refer [supervise]]
   [leela.storaged.network.router :as router]
   [leela.storaged.cassandra.bitmap :as bitmap]
   [leela.storaged.cassandra.metrics :as metrics]
   [leela.storaged.cassandra.sequence :as sequence]
   [leela.storaged.network.controller :as ctrl]
   [leela.storaged.cassandra.connection :as storage])
  (:gen-class))

(defn maybe-getenv [u]
  (if (= \$ (first u))
    (System/getenv (subs u 1))
    u))

(defn parse-args [args]
  (let [[options args banner] (cli args
                                   ["--help" "this message"
                                    :default false
                                    :flag true]
                                   ["--cassandra" "the cassandra hosts [comma separated] to connect to"
                                    :default ["localhost"]
                                    :parse-fn #(split % #",")]
                                   ["--keyspace" "the cassandra keyspace to use"
                                    :default "leela"]
                                   ["--username" "the username to use when connecting to cassandra"
                                    :default "$LEELA_CASSANDRA_USERNAME"]
                                   ["--password" "the password to use when connecting to cassandra"
                                    :default "$LEELA_CASSANDRA_PASSWORD"]
                                   ["--capabilities" "the number of workers to fork"
                                    :default 8
                                    :parse-fn #(Integer/parseInt %)]
                                   ["--queue-size" "the number of outstanding requests"
                                    :default 1024
                                    :parse-fn #(Integer/parseInt %)]
                                   ["--secret" "the shared secret to authenticate requests"
                                    :default "$LEELA_SHARED_SECRET"]
                                   ["--endpoint" "the binding address"
                                    :default "tcp://*:50023"])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    options))

(defn -main [& args]
  (let [options  (parse-args args)
        c-args   {:credentials {:username (maybe-getenv (:username options))
                                :password (maybe-getenv (:password options))}
                  :max-connections 128
                  :connections 1}
        endpoint (:endpoint options)]
    (supervise
     (storage/with-cluster (:cassandra options) c-args
       (storage/with-keyspace (:keyspace options)
         (bitmap/create-schema)
         (metrics/create-schema)
         (sequence/create-schema)
         (with-open [ctx (ZMQ/context 1)]
           (router/router-start ctx {:onjob (ctrl/make-controller-default)
                                     :onerr nil} (assoc options :endpoint endpoint))))))))
