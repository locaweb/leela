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
            [clojure.tools.logging :only [warn]]
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
                                   ["--node" "the nodename to report and also to read config from"
                                    :default (.. java.net.InetAddress getLocalHost getHostName)])]
    (when (:help options)
      (println banner)
      (System/exit 0))

    (config/zk-attach (:node options) (:zookeeper options) (maybe-cat "leela:leela" (:zk-authfile options)))
    (let [cfg (config/read-state :cassandra)]
      (storage/with-session [cluster (get cfg :seed "127.0.0.1") (get cfg :keyspace "leela")]
        (zserver/server-start (ZMQ/context 1) cluster)))))
