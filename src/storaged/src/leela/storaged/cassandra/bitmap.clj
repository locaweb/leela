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

(ns leela.storaged.cassandra.bitmap
  (:require
   [leela.storaged.bytes :as bytes]
   [clojurewerkz.cassaforte.cql :as cql]
   [clojurewerkz.cassaforte.query :as stmt]
   [clojurewerkz.cassaforte.client :as client]
   [leela.storaged.cassandra.config :refer [bitmap-table bitmap-idx-table]]
   [leela.storaged.cassandra.connection :as conn :refer [*limit* *cluster*]]))

(defn create-schema []
  (conn/create-table-ifne bitmap-idx-table
                          (stmt/column-definitions [[:plane   :bigint]
                                                    [:varname :text]
                                                    [:content :text]
                                                    [:version :int]
                                                    [:blocks  (stmt/list-type :ascii)]
                                                    [:primary-key [[:plane :varname :content] :version]]])
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:version :desc]]}))
  (conn/create-table-ifne bitmap-table
                          (stmt/column-definitions [[:hash :ascii]
                                                    [:data :blob]
                                                    [:primary-key [:hash]]])))

(defn store-chunk [hash data]
  (cql/insert *cluster* (conn/fqn bitmap-table)
              {:hash hash
               :data data}))

(defn fetch-chunk [hash]
  (conn/fetch-one #(bytes/bytes-from-bytebuff (:data %))
                  (cql/select *cluster* (conn/fqn bitmap-table)
                              (stmt/columns :data)
                              (stmt/where [[= :hash hash]])
                              (stmt/limit 1))))

(defn store-index [plane varname content version blocks]
  (conn/tx-success?
   (cql/insert *cluster* (conn/fqn bitmap-idx-table)
               {:plane plane
                :varname varname
                :content content
                :version version
                :blocks blocks}
               (stmt/if-not-exists))))

(defn- fetch-index-with- [predicates]
  (cql/select *cluster* (conn/fqn bitmap-idx-table)
              (stmt/columns :version :blocks)
              (stmt/where predicates)
              (stmt/limit *limit*)))

(defn fetch-index
  ([plane varname content]
   (fetch-index-with- [[= :plane plane]
                       [= :varname varname]
                       [= :content content]]))
  ([plane varname content version]
   (fetch-index-with- [[= :plane plane]
                       [= :varname varname]
                       [= :content content]
                       [< :version version]])))
