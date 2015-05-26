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
   [leela.storaged.cassandra.connection :as conn :refer [+limit+ +keyspace+]]))

(def data-table :bitmap)
(def index-table :bitmap_index)

(defn create-schema [cluster]
  (conn/create-table-ifne cluster index-table
                          (stmt/column-definitions {:varname :text
                                                    :content :text
                                                    :version :int
                                                    :chklist (stmt/list-type :ascii)
                                                    :primary-key [[:varname :content] :version]})
                          (stmt/with {:compaction {:class "LeveledCompactionStrategy"
                                                   :sstable_size_in_mb "256"}
                                      :clustering-order [[:version :desc]]}))
  (conn/create-table-ifne cluster data-table
                          (stmt/column-definitions {:hash :ascii
                                                    :data :blob
                                                    :primary-key [:hash]})))

(defn store-chunk [cluster hash data]
  (cql/insert cluster (conn/fqn data-table)
              {:hash hash
               :data data}))

(defn fetch-chunk [cluster hash]
  (conn/fetch-one #(bytes/bytes-from-bytebuff (:data %))
                  (cql/select cluster (conn/fqn data-table)
                              (stmt/columns :data)
                              (stmt/where [[= :hash hash]])
                              (stmt/limit 1))))

(defn store-index [cluster varname content version chklist]
  (conn/tx-success?
   (cql/insert cluster (conn/fqn index-table)
               {:varname varname
                :content content
                :version version
                :chklist chklist}
               (stmt/if-not-exists))))

(defn- fetch-index-with- [cluster predicates]
  (cql/select cluster (conn/fqn index-table)
              (stmt/columns :version :chklist)
              (stmt/where predicates)
              (stmt/limit +limit+)))

(defn fetch-index
  ([cluster varname content]
   (fetch-index-with- cluster [[= :varname varname]
                               [= :content content]]))
  ([cluster varname content version]
   (fetch-index-with- cluster [[= :varname varname]
                               [= :content content]
                               [< :version version]])))
