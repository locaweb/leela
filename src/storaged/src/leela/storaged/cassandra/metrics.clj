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
   [leela.storaged.cassandra.config :refer [metrics-table metrics-idx-table metrics-trigger]]
   [leela.storaged.cassandra.triggers :refer [mtrigger-class]]
   [leela.storaged.cassandra.connection :as conn :refer [*limit* *cluster*]]))

(defn create-schema []
  (conn/create-table-ifne metrics-table
                          (stmt/column-definitions [["schema"  :int]
                                                    [:metric   :int]
                                                    [:bucket   :bigint]
                                                    [:offset   :int]
                                                    [:datum    :blob]
                                                    [:primary-key [["schema" :metric :bucket] :offset]]])
                          (stmt/with {:compaction {:class "SizeTieredCompactionStrategy"
                                                   :cold_reads_to_omit "0"}
                                      :compact-storage :true}))
  (conn/create-table-ifne metrics-idx-table
                          (stmt/column-definitions [["schema"  :int]
                                                    [:metric   :int]
                                                    [:bucket   :bigint]
                                                    [:location :text]
                                                    [:primary-key [["schema" :metric] :bucket :location]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:bucket :desc]]}))
  (conn/create-trigger-ifne metrics-trigger metrics-table mtrigger-class))


(defn index-metric [schema metric bucket location]
  (cql/insert *cluster* (conn/fqn metrics-idx-table)
              {"schema" schema
               :metric metric
               :bucket bucket
               :location location}))

(defn store-metric [schema metric bucket offset datum]
  (cql/insert *cluster* (conn/fqn metrics-table)
              {"schema" schema
               :metric metric
               :bucket bucket
               :offset offset
               :datum datum}))

(defn- fetch-index-with- [predicates]
  (cql/select *cluster* (conn/fqn metrics-idx-table)
              (stmt/columns :bucket :location)
              (stmt/where predicates)
              (stmt/limit *limit*)))

(defn- fetch-metric-with- [predicates]
  (map #(identity {:offset (:offset %)
                   :datum (bytes/bytes-from-bytebuff (:datum %))})
       (cql/select *cluster* (conn/fqn metrics-table)
                   (stmt/columns :offset :datum)
                   (stmt/where predicates)
                   (stmt/limit *limit*))))

(defn fetch-index
  ([schema metric]
   (fetch-index-with- [[= "schema" schema]
                       [= :metric metric]]))
  ([schema metric bucket]
   (fetch-index-with- [[= "schema" schema]
                       [= :metric metric]
                       [< :bucket bucket]])))

(defn fetch-metric
  ([schema metric bucket]
   (fetch-metric-with- [[= "schema" schema]
                        [= :metric metric]
                        [= :bucket bucket]]))
  ([schema metric bucket offset]
   (fetch-metric-with- [[= "schema" schema]
                        [= :metric metric]
                        [= :bucket bucket]
                        [> :offset offset]])))
