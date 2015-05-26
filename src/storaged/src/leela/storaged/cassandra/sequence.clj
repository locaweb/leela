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

(ns leela.storaged.cassandra.sequence
  (:require
   [leela.storaged.bytes :as bytes]
   [clojurewerkz.cassaforte.cql :as cql]
   [clojurewerkz.cassaforte.query :as stmt]
   [clojurewerkz.cassaforte.client :as client]
   [leela.storaged.cassandra.connection :as conn :refer [+limit+ +keyspace+]]))

(def seq-table :sequence)
(def blk-table :sequence_block)
(def idx-table :sequence_index)

(defn create-schema [cluster]
  (conn/create-table-ifne cluster blk-table
                          (stmt/column-definitions {:plane :bigint
                                                    :block :bigint
                                                    :primary-key [[:plane] :block]})
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:block :desc]]}))
  (conn/create-table-ifne cluster seq-table
                          (stmt/column-definitions {:plane :bigint
                                                    :object :blob
                                                    :seqid :bigint
                                                    :primary-key [[:plane :object]]})
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}}))
  
  (conn/create-table-ifne cluster idx-table
                          (stmt/column-definitions {:plane :bigint
                                                    :seqid :bigint
                                                    :object :blob
                                                    :primary-key [[:plane] :seqid]})
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:seqid :desc]]})))

(defn fetch-block [cluster plane]
  (map (partial :block)
       (cql/select cluster (conn/fqn blk-table)
                   (stmt/columns :block)
                   (stmt/where [[= :plane plane]])
                   (stmt/limit +limit+))))

(defn- next-block- [cluster plane]
  (if-let [blk (first (conn/with-limit 1 (fetch-block cluster plane)))]
    blk
    0))

(defn- fetch-idx-with- [cluster predicates]
  (conn/fetch-all #(identity {:seqid (:seqid %)
                              :object (bytes/bytes-from-bytebuff (:object %))})
                  (cql/select cluster (conn/fqn idx-table)
                              (stmt/columns :object :seqid)
                              (stmt/where predicates)
                              (stmt/limit +limit+))))

(defn fetch-idx
  ([cluster plane]
   (fetch-idx-with- cluster [[= :plane plane]]))
  ([cluster plane seqid]
   (fetch-idx-with- cluster [[= :plane plane] [< :seqid seqid]])))

(defn store-block [cluster plane blk]
  (conn/tx-success?
   (cql/insert cluster (conn/fqn blk-table)
               {:plane plane
                :block blk}
               (stmt/if-not-exists))))

(defn alloc-block [cluster plane]
  (loop [blk (next-block- cluster plane)]
    (if (store-block cluster plane blk)
      blk
      (recur (inc blk)))))

(defn store-obj [cluster plane seqid obj]
  (conn/tx-success?
   (cql/insert cluster (conn/fqn seq-table)
               {:plane plane
                :seqid seqid
                :object obj}
               (stmt/if-not-exists))))

(defn index-obj [cluster plane seqid obj]
  (cql/insert cluster (conn/fqn idx-table)
              {:plane plane
               :seqid seqid
               :object obj}))

(defn fetch-obj-by-seqid [cluster plane seqid]
  (conn/fetch-one #(bytes/bytes-from-bytebuff (:object %))
                  (cql/select cluster (conn/fqn idx-table)
                              (stmt/columns :object)
                              (stmt/where [[= :plane plane]
                                           [= :seqid seqid]])
                              (stmt/limit 1))))

(defn fetch-seqid-by-obj [cluster plane obj]
  (conn/fetch-one #(:seqid %)
                  (cql/select cluster (conn/fqn seq-table)
                              (stmt/columns :seqid)
                              (stmt/where [[= :plane plane]
                                           [= :object obj]])
                              (stmt/limit 1))))
