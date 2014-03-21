;; Copyright 2014 (c) Diego Souza <dsouza@c0d3.xxx>
;; Copyright 2014 (c) Alexandre Baaklini <abaaklini@gmail.com>
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
  (:use     [clj-time.core :only [date-time year month day]]
            [clj-time.coerce]
            [clojure.tools.logging :only [info warn]]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [clojure.string :as s]
            [leela.blackbox.f :as f]
            [clojurewerkz.cassaforte.client :as client]))

(def +limit+ 32)

(defn decode-time [time-in-sec]
  (let [time (from-long (* 1000 time-in-sec))
        bucket (quot (to-long (date-time (year time) (month time) (day time))) 1000)
        hitime (quot (- time-in-sec bucket) 3600)
        lotime (rem (- time-in-sec bucket) 3600)]
    [bucket hitime lotime]))

(defn encode-time [bucket hitime lotime]
  (+ bucket (* 3600 hitime) lotime))

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
     (column-definitions {:key :uuid :name :varchar :bucket :bigint :hitime :int :data (map-type :int :blob) :primary-key [[:key :name :bucket] :hitime]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :k_attr)
    (warn "creating table k_attr")
    (create-table
     cluster :k_attr
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :value :blob  :primary-key [[:key :name]]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :g_index)
    (warn "creating table g_index")
    (create-table
     cluster :g_index
     (if-not-exists)
     (column-definitions {:key :uuid :rev :boolean :name :varchar :primary-key [[:key :rev] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :k_index)
    (warn "creating table k_index")
    (create-table
     cluster :k_index
     (if-not-exists)
     (column-definitions {:key :uuid :rev :boolean :name :varchar :primary-key [[:key :rev] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :t_index)
    (warn "creating table t_index")
    (create-table 
    cluster :t_index
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
  (doseq [t [:graph :g_index :k_index :t_index :t_attr :k_attr :n_naming :g_naming]]
    (truncate cluster t)))

(defn fmt-put-index-opts [table data opts]
  (let [value (:name data)]
    (if (empty? opts)
      [(insert-query table (into data {:rev true :name (s/reverse value)}))
       (insert-query table (into data {:rev false}))]
      [(insert-query table (into data {:rev true :name (s/reverse value)}) (apply using opts))
       (insert-query table (into data {:rev false}) (apply using opts))])))

(defn fmt-put-index [table data]
  (fmt-put-index-opts table data []))

(defn fmt-del-index [table data]
  (let [value (:name data)]
    [(delete-query table (where :key (:key data) :rev true :name (s/reverse value)))
     (delete-query table (where :key (:key data) :rev false :name value))]))

(defn fmt-put-link [data]
  (insert-query :graph data))

(defn fmt-del-link [data]
  (if-let [b (:b data)]
    (delete-query :graph (where :a (:a data) :l (:l data) :b b))
    (delete-query :graph (where :a (:a data) :l (:l data)))))

(defn fmt-put-kattr [[data opts0]]
  (let [opts (flatten (seq (dissoc opts0 :index)))
        idx (if (:index opts0)
              (fmt-put-index-opts :k_index
                                  {:key (:key data)
                                   :name (:name data)} opts)
              [])]
    (if (empty? opts)
      (cons (insert-query :k_attr data) idx)
      (cons (insert-query :k_attr data (apply using opts)) idx))))

(defn fmt-put-tattr [[data opts0]]
  (let [opts (flatten (seq (dissoc opts0 :index)))
        [bucket hitime lotime] (decode-time (:time data))
        idx (if (:index opts0)
              (fmt-put-index-opts :t_index
                                  {:key (:key data)
                                   :name (:name data)} opts)
              [])]
    (if (empty? opts)
      (cons (update-query :t_attr
                          {:data [+ {lotime (:value data)}]}
                          (where
                           :key (:key data)
                           :name (:name data)
                           :bucket bucket
                           :hitime hitime)) idx)
      (cons (update-query :t_attr
                          {:data [+ {lotime (:value data)}]}
                          (where
                           :key (:key data)
                           :name (:name data)
                           :bucket bucket
                           :hitime hitime)
                           (apply using opts)) idx))))

(defn fmt-del-kattr [data]
  (cons (delete-query :k_attr (where :key (:key data) :name (:name data)))
        (fmt-del-index :k_index {:key (:key data)
                                 :name (:name data)})))

(defn fmt-del-tattr [data]
  (if (contains? data :time)
    (let [[bucket hitime lotime] (decode-time (:time data))]
      [(delete-query
        :t_attr
        (columns {:data lotime})
        (where :key (:key data)
               :name (:name data)
               :bucket bucket
               :hitime hitime))])
    (fmt-del-index :t_index {:key (:key data)
                             :name (:name data)})))

(defn put-index [cluster table indexes]
  (let [query (->> indexes
                   (mapcat (partial fmt-put-index table))
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

(defn get-index [cluster table k rev & optional]
  (let [[start finish] optional
        reseq (fn [arg] (if rev (s/reverse arg) arg))]
    (map #(reseq (:name %))
         (select cluster
                 table
                 (columns :name)
                 (case [(boolean start) (boolean finish)]
                   [true true] (where :key k :rev rev :name [:>= (reseq start)] :name [:< (reseq finish)])
                   [true false] (where :key k :rev rev :name [:>= (reseq start)])
                   (where :key k :rev rev))
                 (limit +limit+)))))

(defn has-index [cluster table k rev name]
  (map #(:name %) (select cluster
                          table
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

(defn put-tattr [cluster attrs]
  (let [query (->> attrs
                   (mapcat fmt-put-tattr)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))

(defn get-tattr [cluster k name time]
  (let [[bucket hitime] (take 2 (decode-time time))]
    (mapcat
     (fn [row]
       (map
        (fn [[lotime value]]
          [(encode-time bucket (:hitime row) lotime) (f/binary-to-bytes value)])
        (:data row)))
     (select cluster
             :t_attr
             (columns :hitime :data)
             (where :key k :name name :bucket bucket :hitime [:>= hitime])))))

(defn del-tattr [cluster attrs]
  (let [query (->> attrs
                   (mapcat fmt-del-tattr)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))

(defn put-kattr [cluster attrs]
  (let [query (->> attrs
                   (mapcat fmt-put-kattr)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))

(defn get-kattr [cluster k name]
  (first
   (map #(f/binary-to-bytes (:value %))
        (select cluster
                :k_attr
                (columns :value)
                (where :key k :name name)
                (limit 1)))))

(defn del-kattr [cluster attrs]
  (let [query (->> attrs
                   (mapcat fmt-del-kattr)
                   (apply queries)
                   (batch-query (logged false))
                   client/render-query)]
    (client/execute cluster query)))
