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
                                                    [:data :ascii]
                                                    [:primary-key [:hash]]])))

(defn store-chunk [hash data]
  (cql/insert *cluster* (conn/fqn bitmap-table)
              {:hash hash
               :data data}))

(defn fetch-chunk [hash]
  (conn/fetch-one #(:data %)
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
