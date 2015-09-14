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

(def *cluster*)

(def *keyspace*)

(def ^:dynamic *limit* 100)

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

(defmacro with-cluster [endpoint options & body]
  `(let [cluster# (client/build-cluster {:hosts ~endpoint
                                        :retry-policy (policies/logging-retry-policy (policies/retry-policy :fallthrough))
                                        :credentials (:credentials ~options)
                                        :load-balancing-policy (policies/token-aware-policy (policies/round-robin-policy))
                                        :connections-per-host (:connections ~options)
                                        :max-connections-per-host (:max-connections ~options)})]
    (info (format "cassandra/with-connection %s" (dissoc ~options :credentials)))
    (with-redefs [*cluster* (.connect cluster#)]
      (try
        ~@body
        (finally
          (client/disconnect *cluster*)
          (client/shutdown-cluster cluster#))))))

(defmacro with-consistency [tag & body]
  `(policies/with-consistency-level (policies/consistency-level ~tag)
     ~@body))

(defmacro with-keyspace [name & body]
  `(with-redefs [*keyspace* ~name] ~@body))

(defmacro with-limit [limit & body]
  `(binding [*limit* ~limit]
     ~@body))
