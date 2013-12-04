;; This file is part of Leela.
;;
;; Leela is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Leela is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Leela.  If not, see <http://www.gnu.org/licenses/>.

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
                                    :default "tcp://*:50021"])]
    (when (:help options)
      (println banner)
      (System/exit 0))

    (let [cassandra-args {:credentials {:username (maybe-getenv (:username options))
                                        :password (maybe-getenv (:password options))}
                          :connections (:capabilities options)}]
      (f/supervise
       (storage/with-session [cluster (:cassandra options) (:keyspace options) cassandra-args]
         (zserver/server-start (ZMQ/context 1) cluster options))))))
