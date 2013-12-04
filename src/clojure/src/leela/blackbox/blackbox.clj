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

(defn maybe-cat [default f]
  (if (and f (.canRead (java.io.File. f)))
    (first (clojure.string/split (slurp f) #"\n"))
    default))

(defn -main [& args]
  (let [[options args banner] (cli args
                                   ["--help" "this message"
                                    :default false
                                    :flag true]
                                   ["--cassandra" "the cassandra hosts [comma separated] to connect to"
                                    :default "localhost"]
                                   ["--keyspace" "the keyspace to use"
                                    :default "leela"]
                                   ["--capabilities" "the number of workers to fork"
                                    :default 64]
                                   ["--queue-size" "the number of outstanding requests"
                                    :default 32]
                                   ["--endpoint" "the binding address"
                                    :default "tcp://*:50021"])]
    (when (:help options)
      (println banner)
      (System/exit 0))

    (f/supervise
     (storage/with-session [cluster (string/split (:cassandra options) ",") (:keyspace options)]
       (zserver/server-start (ZMQ/context 1) cluster options)))))
