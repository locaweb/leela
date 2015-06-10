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
   [leela.storaged.cassandra.config :refer [sequence-table sequence-idx-table sequence-blk-table sequence-trigger]]
   [leela.storaged.cassandra.triggers :refer [strigger-class]]
   [leela.storaged.cassandra.connection :as conn :refer [*limit* *cluster*]]))

(defn create-schema []
  (conn/create-table-ifne sequence-blk-table
                          (stmt/column-definitions [[:plane :bigint]
                                                    [:block :bigint]
                                                    [:primary-key [[:plane] :block]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:block :desc]]}))
  (conn/create-table-ifne sequence-table
                          (stmt/column-definitions [[:plane :bigint]
                                                    [:object :blob]
                                                    [:seqid :bigint]
                                                    [:primary-key [[:plane :object]]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}}))
  (conn/create-table-ifne sequence-idx-table
                          (stmt/column-definitions [[:plane :bigint]
                                                    [:seqid :bigint]
                                                    [:object :blob]
                                                    [:primary-key [[:plane] :seqid]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:seqid :desc]]})))


(defn fetch-block [plane]
  (map (partial :block)
       (cql/select *cluster* (conn/fqn sequence-blk-table)
                   (stmt/columns :block)
                   (stmt/where [[= :plane plane]])
                   (stmt/limit *limit*))))

(defn- next-block- [plane]
  (if-let [blk (first (conn/with-limit 1 (fetch-block plane)))]
    blk
    0))

(defn- fetch-idx-with- [predicates]
  (conn/fetch-all #(identity {:seqid (:seqid %)
                              :object (bytes/bytes-from-bytebuff (:object %))})
                  (cql/select *cluster* (conn/fqn sequence-idx-table)
                              (stmt/columns :object :seqid)
                              (stmt/where predicates)
                              (stmt/limit *limit*))))

(defn fetch-idx
  ([plane]
   (fetch-idx-with- [[= :plane plane]]))
  ([plane seqid]
   (fetch-idx-with- [[= :plane plane] [< :seqid seqid]])))

(defn store-block [plane blk]
  (conn/tx-success?
   (cql/insert *cluster* (conn/fqn sequence-blk-table)
               {:plane plane
                :block blk}
               (stmt/if-not-exists))))

(defn alloc-block [plane]
  (loop [blk (next-block- plane)]
    (if (store-block plane blk)
      blk
      (recur (inc blk)))))

(defn store-obj [plane seqid obj]
  (conn/tx-success?
   (cql/insert *cluster* (conn/fqn sequence-table)
               {:plane plane
                :seqid seqid
                :object obj}
               (stmt/if-not-exists))))

(defn index-obj [plane seqid obj]
  (cql/insert *cluster* (conn/fqn sequence-idx-table)
              {:plane plane
               :seqid seqid
               :object obj}))

(defn store-obj-n-idx [plane seqid obj]
  (if-not (store-obj plane seqid obj)
    false
    (do
      (index-obj plane seqid obj)
      true)))

(defn fetch-obj-by-seqid [plane seqid]
  (conn/fetch-one #(bytes/bytes-from-bytebuff (:object %))
                  (cql/select *cluster* (conn/fqn sequence-idx-table)
                              (stmt/columns :object)
                              (stmt/where [[= :plane plane]
                                           [= :seqid seqid]])
                              (stmt/limit 1))))

(defn fetch-seqid-by-obj [plane obj]
  (conn/fetch-one #(:seqid %)
                  (cql/select *cluster* (conn/fqn sequence-table)
                              (stmt/columns :seqid)
                              (stmt/where [[= :plane plane]
                                           [= :object obj]])
                              (stmt/limit 1))))
