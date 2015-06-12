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
                                                    [:block :int]
                                                    [:primary-key [[:plane] :block]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:block :desc]]}))
  (conn/create-table-ifne sequence-table
                          (stmt/column-definitions [[:plane :bigint]
                                                    [:seqid :int]
                                                    [:object :blob]
                                                    [:primary-key [[:object] :plane]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}}))
  (conn/create-index-ifne sequence-table "plane"
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
  (conn/fetch-one :seqid
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
