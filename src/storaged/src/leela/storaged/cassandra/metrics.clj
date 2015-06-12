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
                          (stmt/column-definitions [[:plane  :bigint]
                                                    [:metric :int]
                                                    [:bucket :bigint]
                                                    [:offset :int]
                                                    [:datum  :blob]
                                                    [:primary-key [[:plane :metric :bucket] :offset]]])
                          (stmt/with {:compaction {:class "SizeTieredCompactionStrategy"
                                                   :cold_reads_to_omit "0"}
                                      :compact-storage :true}))
  (conn/create-table-ifne metrics-idx-table
                          (stmt/column-definitions [[:plane    :bigint]
                                                    [:metric   :int]
                                                    [:bucket   :bigint]
                                                    [:location :text]
                                                    [:primary-key [[:plane :metric] :bucket :location]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:bucket :desc]]}))
  (conn/create-trigger-ifne metrics-trigger metrics-table mtrigger-class))

(defn store-metric [plane metric bucket offset datum]
  (cql/insert *cluster* (conn/fqn metrics-table)
              {:plane  plane
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
  ([plane metric]
   (fetch-index-with- [[= :plane plane]
                       [= :metric metric]]))
  ([plane metric bucket]
   (fetch-index-with- [[= :plane plane]
                       [= :metric metric]
                       [< :bucket bucket]])))

(defn fetch-metric
  ([plane metric bucket]
   (fetch-metric-with- [[= :plane plane]
                        [= :metric metric]
                        [= :bucket bucket]]))
  ([plane metric bucket offset]
   (fetch-metric-with- [[= :plane plane]
                        [= :metric metric]
                        [= :bucket bucket]
                        [> :offset offset]])))
