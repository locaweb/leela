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
    (create-keyspace
     cluster
     keyspace
     (if-not-exists)
     (with {:replication {:class "SimpleStrategy" :replication_factor 1}})))
  (info (format "connecting to keyspace %s" keyspace))
  (use-keyspace cluster keyspace)
  (when-not (describe-table cluster keyspace :graph)
    (warn "creating table graph")
    (create-table
     cluster :graph
     (if-not-exists)
     (column-definitions {:a :uuid :l :varchar :b :uuid :primary-key [[:a :l] :b]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :n_naming)
    (warn "creating table n_naming")
    (create-table
     cluster :n_naming
     (if-not-exists)
     (column-definitions {:user :varchar :tree :varchar :node :varchar :guid :uuid :primary-key [[:user :tree] :node]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :g_naming)
    (warn "creating table g_naming")
    (create-table
     cluster :g_naming
     (if-not-exists)
     (column-definitions {:guid :uuid :user :varchar :tree :varchar :node :varchar :primary-key [:guid]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :t_attr)
    (warn "creating table t_attr")
    (create-table
     cluster :t_attr
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :slot :int :value :blob :primary-key [[:key :name] :slot]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :k_attr)
    (warn "creating table k_attr")
    (create-table
     cluster :k_attr
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :value :blob  :primary-key [[:key :name]]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :search)
    (warn "creating table search")
    (create-table
     cluster :search
     (if-not-exists)
     (column-definitions {:key :uuid :rev :boolean :name :varchar :primary-key [[:key :rev] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}}))))

(defmacro with-connection [[conn endpoint options] & body]
  `(let [~conn (client/connect (client/build-cluster {:contact-points ~endpoint
                                                      :credentials (:credentials ~options)
                                                      :load-balancing-policy (client/token-aware-policy (client/round-robin-policy))
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
  (doseq [t [:graph :search :t_attr :k_attr :n_naming :g_naming]]
    (truncate cluster t)))

(defn fmt-put-index [data]
  (let [value (:name data)]
    [(insert-query :search (into {:rev true :name (s/reverse value)} data))
     (insert-query :search (into {:rev false} data))]))

(defn fmt-put-link [data]
  (insert-query :graph data))

(defn fmt-del-link [[a l b]]
  (if b
    (delete-query :graph (where :a a :l l :b b))
    (delete-query :graph (where :a a :l l))))

(defn putindex [cluster indexes]
  (let [query (->> indexes
                   (mapcat fmt-put-index)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))

(defn getguid [cluster user tree node]
  (first (map #(:guid %)
              (select cluster
                      :n_naming
                      (columns :guid)
                      (where :user user :tree tree :node node)
                      (limit 1)))))

(defn getname [cluster guid]
  (first (map (fn [row] [(:user row) (:tree row) (:node row)])
              (select cluster
                      :g_naming
                      (columns :user :tree :node)
                      (where :guid guid)
                      (limit 1)))))

(defn putguid [cluster user tree node]
  (if-let [guid (getguid cluster user tree node)]
    guid
    (let [guid (f/uuid-1)]
      (client/execute cluster
                      (client/render-query
                       (batch-query
                        (queries (insert-query :n_naming {:user user :tree tree :node node :guid guid})
                                 (insert-query :g_naming {:user user :tree tree :node node :guid guid})))))
      guid)))

(defn getindex [cluster k rev & optional]
  (let [[start finish] optional
        reseq (fn [arg] (if rev (s/reverse arg) arg))]
    (map #(reseq (:name %))
         (select cluster
                 :search
                 (columns :name)
                 (case [(boolean start) (boolean finish)]
                   [true true] (where :key k :rev rev :name [:>= (reseq start)] :name [:< (reseq finish)])
                   [true false] (where :key k :rev rev :name [:>= (reseq start)])
                   (where :key k :rev rev))
                 (limit +limit+)))))

(defn hasindex [cluster k rev name]
  (map #(:name %) (select cluster
                          :search
                          (columns :name)
                          (where :key k :rev rev :name name)
                          (limit 1))))

(defn putlink [cluster links]
  (let [query (->> links
                   (map fmt-put-link)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))

(defn dellink [cluster links]
  (let [query (->> links
                   (map fmt-del-link)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))

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
          :t_attr {:key k :name name :slot slot :value value}))

(defn get-tattr [cluster k name]
  (map (fn [row] [(:slot row) (f/binary-to-bytes (:value row))])
       (select cluster
               :t_attr
               (columns :slot :value)
               (where :key k :name name)
               (limit +limit+))))

(defn del-tattr [cluster k name slot]
  (delete cluster
          :t_attr
          (where :key k :name name :slot slot)))

(defn put-kattr [cluster k name value]
  (insert cluster
          :k_attr {:key k :name name :value value}))

(defn get-kattr [cluster k name]
  (first
   (map #(f/binary-to-bytes (:value %))
        (select cluster
                :k_attr
                (columns :value)
                (where :key k :name name)
                (limit 1)))))

(defn del-kattr [cluster k name]
  (delete cluster
          :k_attr
          (where :key k :name name)))
