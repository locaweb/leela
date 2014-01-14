;; Copyright 2013 (c) Diego Souza <dsouza@c0d3.xxx>
;; Copyright 2013 (c) Alexandre Baaklini <abaaklini@gmail.com>
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

(ns leela.blackbox.storage.cassandra
  (:use     [clojure.tools.logging :only [info warn]]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [clojure.string :as s]
            [leela.blackbox.f :as f]
            [clojurewerkz.cassaforte.client :as client]))

(def +limit+ 256)

(defn check-schema [cluster keyspace]
  (when-not (describe-keyspace cluster keyspace)
    (warn (format "creating keyspace %s [simplestrategy, rf=1]" keyspace))
    (create-keyspace cluster keyspace (with {:replication {:class "SimpleStrategy" :replication_factor 1}})))
  (info (format "connecting to keyspace %s" keyspace))
  (use-keyspace cluster keyspace)
  (when-not (describe-table cluster keyspace :graph)
    (warn "creating table graph")
    (create-table
     cluster :graph
     (column-definitions {:a :uuid :l :varchar :b :uuid :primary-key [[:a :l] :b]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :naming)
    (warn "creating table naming")
    (create-table
     cluster :naming
     (column-definitions {:user :varchar :tree :varchar :node :varchar :guid :uuid :primary-key [[:user :tree] :node]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :tattr)
    (warn "creating table tattr")
    (create-table
     cluster :tattr
     (column-definitions {:key :uuid :name :varchar :slot :int :value :blob :primary-key [[:key :name] :slot]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :kattr)
    (warn "creating table kattr")
    (create-table
      cluster :kattr
      (column-definitions {:key :uuid :name :varchar :value :blob  :primary-key [[:key :name]]})
      (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :search)
    (warn "creating table search")
    (create-table
     cluster
     :search
     (column-definitions {:key :uuid :code :int :name :varchar :primary-key [[:key :code] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}}))))

(defmacro with-connection [[conn endpoint options] & body]
  `(let [~conn (client/connect (client/build-cluster {:contact-points ~endpoint
                                                      :credentials (:credentials ~options)
                                                      :max-connections-per-host (:connections ~options)}))]
     (try
       ~@body
       (finally (.shutdown ~conn)))))

(defmacro with-session [[cluster endpoint keyspace & options] & body]
  `(with-connection [~cluster ~endpoint ~(first options)]
     (check-schema ~cluster ~keyspace)
     ~@body))

(defmacro with-consistency [tag & body]
  `(client/with-consistency-level
     (client/consistency-level ~tag) ~@body))

(defmacro with-limit [lim & body]
  `(with-redefs [+limit+ (or ~lim +limit+)]
     ~@body))

(defn truncate-all [cluster]
  (doseq [t [:graph :search :tattr :kattr :naming]]
    (truncate cluster t)))

(defn putindex [cluster k code name]
  (insert cluster :search {:key k :code code :name name}))

(defn getguid [cluster user tree node]
  (first (map #(:guid %)
              (select cluster
                      :naming
                      (columns :guid)
                      (where :user user :tree tree :node node)
                      (limit 1)))))

(defn putguid [cluster user tree node]
  (with-consistency :serial
    (insert cluster :naming {:user user :tree tree :node node :guid (f/uuid-1)} (if-not-exists))
    (getguid cluster user tree node)))

(defn getindex [cluster k code & optional]
  (let [[start finish] optional]
    (map #(:name %) (select cluster
                            :search
                            (columns :name)
                            (case [(boolean start) (boolean finish)]
                              [true true] (where :key k :code code :name [:>= start] :name [:< finish])
                              [true false] (where :key k :code code :name [:>= start])
                              (where :key k :code code))
                            (limit +limit+)))))

(defn hasindex [cluster k code name]
  (map #(:name %) (select cluster
                          :search
                          (columns :name)
                          (where :key k :code code :name name)
                          (limit 1))))

(defn putlink [cluster a l b]
  (insert cluster :graph {:a a :l l :b b}))

(defn dellink [cluster a l & b]
  (delete cluster
           :graph
           (if (seq b)
             (where :a a :l l :b (first b))
             (where :a a :l l))))

(defn getlink [cluster k l & page]
  (map #(:b %) (select cluster
                       :graph
                       (columns :b)
                       (if (seq page)
                         (where :a k :l l :b [:>= (first page)])
                         (where :a k :l l))
                       (limit +limit+))))

(defn put-tattr [cluster k name slot value]
  (insert cluster
          :tattr {:key k :name name :slot slot :value value}))

(defn get-tattr [cluster k name]
  (map (fn [row] [(:slot row) (f/binary-to-bytes (:value row))])
       (select cluster
               :tattr
               (columns :slot :value)
               (where :key k :name name)
               (limit +limit+))))

(defn del-tattr [cluster k name slot]
  (delete cluster
          :tattr
          (where :key k :name name :slot slot)))

(defn put-kattr [cluster k name value]
  (insert cluster
          :kattr {:key k :name name :value value}))

(defn get-kattr [cluster k name]
  (first
   (map #(f/binary-to-bytes (:value %))
        (select cluster
                :kattr
                (columns :value)
                (where :key k :name name)
                (limit 1)))))

(defn del-kattr [cluster k name]
  (delete cluster
          :kattr
          (where :key k :name name)))
