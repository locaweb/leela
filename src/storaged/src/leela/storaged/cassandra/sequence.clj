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
   [leela.storaged.cassandra.config :refer [sequence-table sequence-blk-table]]
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
                                                    [:seqid :bigint]
                                                    [:object :blob]
                                                    [:primary-key [[:object] :plane]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}}))
  (conn/create-index-ifne sequence-table :plane
                          (stmt/index-name :sequence_plane)))

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

(defn- fetch-sequence-with- [predicates]
  (conn/fetch-all #(identity {:seqid (:seqid %)
                              :object (bytes/bytes-from-bytebuff (:object %))})
                  (cql/select *cluster* (conn/fqn sequence-table)
                              (stmt/columns :object :seqid)
                              (stmt/where predicates)
                              (stmt/limit *limit*))))

(defn fetch-sequence
  ([plane]
   (fetch-sequence-with- [[= :plane plane]]))
  ([plane object]
   (fetch-sequence-with- [[= :plane plane]
                          [> (stmt/token :object) (stmt/token object)]])))

(defn fetch-seqid [plane obj]
  (conn/fetch-one #(:seqid %)
                  (cql/select *cluster* (conn/fqn sequence-table)
                              (stmt/columns :seqid)
                              (stmt/where [[= :object obj]
                                           [= :plane plane]])
                              (stmt/limit 1))))

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
