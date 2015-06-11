;; Copyright 2015 (c) Diego Souza <dsouza@c0d3.xxx>
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

(ns leela.storaged.cassandra.connection
  (:require
   [qbits.hayt :refer [->raw]]
   [clojure.string :refer [split]]
   [clojure.tools.logging :refer [info warn]]
   [qbits.hayt.dsl.statement :refer [create-trigger]]
   [clojurewerkz.cassaforte.cql :as cql]
   [clojurewerkz.cassaforte.query :as stmt]
   [clojurewerkz.cassaforte.client :as client]
   [clojurewerkz.cassaforte.policies :as policies]))

(def ^:dynamic *limit* 100)

(def ^:dynamic *cluster*)

(def ^:dynamic *keyspace*)

(defn rfqn [keyspace name]
  (stmt/cql-ns keyspace name))

(defn fqn [name]
  (rfqn *keyspace* name))

(defn fetch-one [fun rows]
  (when-first [ans rows]
    (fun ans)))

(defn fetch-all [fun rows]
  (map (comp fun) rows))

(defn tx-success? [rows]
  (fetch-one #((keyword "[applied]") %) rows))

(defn desc-tables []
  (fetch-all #(:columnfamily_name %)
             (cql/select *cluster* (rfqn :system :schema_columnfamilies)
                         (stmt/columns :columnfamily_name)
                         (stmt/where [[= :keyspace_name *keyspace*]]))))

(defn has-trigger? [cfname name]
  (seq (cql/select *cluster* (rfqn :system :schema_triggers)
                   (stmt/columns :trigger_name)
                   (stmt/where [[= :keyspace_name *keyspace*]
                                [= :columnfamily_name cfname]
                                [= :trigger_name name]]))))

(defn has-index? [cfname cname]
  (fetch-one :index_name (cql/select *cluster* (rfqn :system :schema_columns)
                                     (stmt/columns :index_name)
                                     (stmt/where [[= :keyspace_name *keyspace*]
                                                  [= :columnfamily_name cfname]
                                                  [= :column_name cname]]))))

(defmacro create-table-ifne [table & body]
  `(when-not (cql/describe-table *cluster* *keyspace* ~table)
     (warn (format "creating table %s.%s" *keyspace* ~table))
     (cql/create-table *cluster* (conn/fqn ~table) (stmt/if-not-exists) ~@body)))

(defmacro create-trigger-ifne [name table clazz]
  `(when-not (has-trigger? ~table ~name)
     (warn (format "creating trigger on %s.%s using %s" *keyspace* ~table (last (split ~clazz #"\."))))
     (client/execute *cluster* (->raw (into (create-trigger ~name (fqn ~table) ~clazz) (stmt/if-not-exists))))))

(defmacro create-index-ifne [table column & body]
  `(when-not (has-index? ~table ~column)
     (warn (format "creating index on %s.%s (%s)" *keyspace* ~table ~column))
     (cql/create-index *cluster* (conn/fqn ~table) ~column (stmt/if-not-exists) ~@body)))

(defmacro with-connection [[conn endpoint options] & body]
  `(let [~conn (.connect (client/build-cluster {:hosts ~endpoint
                                                :retry-policy (policies/logging-retry-policy (policies/retry-policy :fallthrough))
                                                :credentials (:credentials ~options)
                                                :load-balancing-policy (policies/token-aware-policy (policies/round-robin-policy))
                                                :connections-per-host (:connections ~options)
                                                :max-connections-per-host (:max-connections ~options)}))]
     (info (format "cassandra/with-connection %s" (dissoc ~options :credentials)))
     (try
       ~@body
       (finally (.close ~conn)))))

(defmacro with-consistency [tag & body]
  `(policies/with-consistency-level (policies/consistency-level ~tag)
     ~@body))

(defmacro with-cluster [cluster & body]
  `(binding [*cluster* ~cluster] ~@body))

(defmacro with-keyspace [name & body]
  `(binding [*keyspace* ~name] ~@body))

(defmacro with-limit [limit & body]
  `(binding [*limit* ~limit]
     ~@body))
