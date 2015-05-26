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

(ns leela.storaged.cassandra.metrics
  (:require
   [leela.storaged.bytes :as bytes]
   [clojurewerkz.cassaforte.cql :as cql]
   [clojurewerkz.cassaforte.query :as stmt]
   [clojurewerkz.cassaforte.client :as client]
   [leela.storaged.cassandra.connection :as conn :refer [+limit+ +keyspace+]]))

(def data-table :metrics)
(def index-table :metric_index)

(defn create-schema [cluster]
  (conn/create-table-ifne cluster data-table
                          (stmt/column-definitions {"schema" :int
                                                    :metric  :int
                                                    :bucket  :bigint
                                                    :offset  :int
                                                    :datum   :blob
                                                    :primary-key [["schema" :metric :bucket] :offset]})
                          (stmt/with {:compaction {:class "SizeTieredCompactionStrategy"
                                                   :cold_reads_to_omit "0"}
                                      :compact-storage :true}))
  (conn/create-table-ifne cluster index-table
                          (stmt/column-definitions {"schema"  :int
                                                    :metric   :int
                                                    :bucket   :bigint
                                                    :location :text
                                                    :primary-key [["schema" :metric] :bucket :location]})
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:bucket :desc]]})))

(defn index-metric [cluster schema metric bucket location]
  (cql/insert cluster (conn/fqn index-table)
              {"schema" schema
               :metric metric
               :bucket bucket
               :location location}))

(defn store-metric [cluster schema metric bucket offset datum]
  (cql/insert cluster (conn/fqn data-table)
              {"schema" schema
               :metric metric
               :bucket bucket
               :offset offset
               :datum datum}))

(defn- fetch-index-with- [cluster predicates]
  (cql/select cluster (conn/fqn index-table)
              (stmt/columns :bucket :location)
              (stmt/where predicates)
              (stmt/limit +limit+)))

(defn- fetch-metric-with- [cluster predicates]
  (cql/select cluster (conn/fqn data-table)
              (stmt/columns :offset :datum)
              (stmt/where predicates)
              (stmt/limit +limit+)))

(defn fetch-index
  ([cluster schema metric]
   (fetch-index-with- cluster [[= "schema" schema]
                               [= :metric metric]]))
  ([cluster schema metric bucket]
   (fetch-index-with- cluster [[= "schema" schema]
                               [= :metric metric]
                               [< :bucket bucket]])))

(defn fetch-metric
  ([cluster schema metric bucket]
   (fetch-metric-with- cluster [[= "schema" schema]
                                [= :metric metric]
                                [= :bucket bucket]]))
  ([cluster schema metric bucket offset]
   (fetch-metric-with- cluster [[= "schema" schema]
                                [= :metric metric]
                                [= :bucket bucket]
                                [> :offset offset]])))
