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
            [clojurewerkz.cassaforte.cql])
  (:require [clojure.string :as s]
            [leela.blackbox.f :as f]
            [clojurewerkz.cassaforte.policies :as policies]
            [clojurewerkz.cassaforte.client :as client]))

(def +limit+ 32)

(defn decode-time [time-in-sec]
  (let [time (from-long (* 1000 time-in-sec))
        bucket (quot (to-long (date-time (year time) (month time) (day time))) 1000)
        slot (- time-in-sec bucket)]
    [(month time) bucket slot]))

(defn encode-time [bucket slot]
  (+ bucket slot))

(defn t-attr-colfam [month]
  (keyword (format "t_attr_%02d" month)))

(defn use-attr-schema [cluster keyspace]
  (when-not (describe-keyspace cluster keyspace)
    (warn (format "creating keyspace %s [simplestrategy, rf=1]" keyspace))
    (create-keyspace
     cluster
     keyspace
     (if-not-exists)
     (with {:replication {:class "SimpleStrategy" :replication_factor 1}})))
  (info (format "connecting to keyspace %s" keyspace))
  (use-keyspace cluster keyspace)
  (dotimes [m 12]
    (let [colfam (t-attr-colfam (+ 1 m))]
      (when-not (describe-table cluster keyspace colfam)
        (warn (format "creating table %s" colfam)
              (create-table
               cluster colfam
               (if-not-exists)
               (column-definitions {:key :uuid :name :varchar :bucket :bigint :slot :int :data :blob :primary-key [[:key :name :bucket] :slot]})
               (with {:compaction {:class "SizeTieredCompactionStrategy" :min_threshold "6"}}))))))
  (when-not (describe-table cluster keyspace :k_attr)
    (warn "creating table k_attr")
    (create-table
     cluster :k_attr
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :value :blob  :primary-key [[:key :name]]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}})))
  (when-not (describe-table cluster keyspace :k_index)
    (warn "creating table k_index")
    (create-table
     cluster :k_index
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :primary-key [[:key] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}})))
  (when-not (describe-table cluster keyspace :t_index)
    (warn "creating table t_index")
    (create-table
    cluster :t_index
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :primary-key [[:key] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}}))))

(defn use-graph-schema [cluster keyspace]
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
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}})))
  (when-not (describe-table cluster keyspace :n_naming)
    (warn "creating table n_naming")
    (create-table
     cluster :n_naming
     (if-not-exists)
     (column-definitions {:user :varchar :tree :varchar :kind :varchar :node :varchar :guid :uuid :primary-key [[:user :tree :kind] :node]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}})))
  (when-not (describe-table cluster keyspace :g_naming)
    (warn "creating table g_naming")
    (create-table
     cluster :g_naming
     (if-not-exists)
     (column-definitions {:guid :uuid :user :varchar :tree :varchar :kind :varchar :node :varchar :primary-key [:guid]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}})))
  (when-not (describe-table cluster keyspace :g_index)
    (warn "creating table g_index")
    (create-table
     cluster :g_index
     (if-not-exists)
     (column-definitions {:key :uuid :name :varchar :primary-key [[:key] :name]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "256"}}))))

(defmacro with-connection [[conn endpoint options] & body]
  `(let [~conn (.connect (client/build-cluster {:hosts ~endpoint
                                                :retry-policy (policies/logging-retry-policy (policies/retry-policy :fallthrough))
                                                :credentials (:credentials ~options)
                                                :load-balancing-policy (policies/token-aware-policy (policies/round-robin-policy))
                                                :connections-per-host (:connections ~options)
                                                :max-connections-per-host (:max-connections ~options)}))]
     (info (format "cassandra/with-connection %s" (dissoc ~options :credentials)))
     (try
       ~@body
       (finally (.close ~conn)))))

(defmacro with-consistency [tag & body]
  `(policies/with-consistency-level
     (policies/consistency-level ~tag) ~@body))

(defmacro with-limit [lim & body]
  `(with-redefs [+limit+ (f/parse-int (or ~lim +limit+))]
     ~@body))

(defn truncate-all [cluster]
  (dotimes [m 12]
    (truncate cluster (t-attr-colfam (+ 1 m))))
  (doseq [t [:graph :g_index :k_index :t_index :k_attr :n_naming :g_naming]]
    (truncate cluster t)))

(defn fmt-put-index-opts [table data opts]
  (let [value (:name data)]
    (if (empty? opts)
      (insert-query table data)
      (insert-query table data (apply using opts)))))

(defn fmt-put-index [table data]
  (fmt-put-index-opts table data []))

(defn fmt-del-index [table data]
  (let [value (:name data)]
    [(delete-query table (where [[= :key (:key data)] [= :name value]]))]))

(defn fmt-put-link [data]
  (insert-query :graph data))

(defn fmt-del-link [data]
  (if-let [b (:b data)]
    (delete-query :graph (where [[= :a (:a data)] [= :l (:l data)] [= :b b]]))
    (delete-query :graph (where [[= :a (:a data)] [= :l (:l data)]]))))

(defn fmt-put-kattr [[data opts0]]
  (let [opts (flatten (seq (dissoc opts0 :index)))
        idx (if (:index opts0)
              [(fmt-put-index-opts :k_index
                                   {:key (:key data)
                                    :name (:name data)} opts)]
              [])]
    (if (empty? opts)
      (cons (insert-query :k_attr data) idx)
      (cons (insert-query :k_attr data (apply using opts)) idx))))

(defn fmt-put-tattr [[data opts0]]
  (let [opts (flatten (seq (dissoc opts0 :index)))
        [month bucket slot] (decode-time (:time data))
        idx (if (:index opts0)
              [(fmt-put-index-opts :t_index
                                   {:key (:key data)
                                    :name (:name data)} opts)]
              [])]
    (if (empty? opts)
      (cons (insert-query (t-attr-colfam month)
                          {:data (:value data)
                           :key (:key data)
                           :name (:name data)
                           :bucket bucket
                           :slot slot}) idx)
      (cons (insert-query (t-attr-colfam month)
                          {:data (:value data)
                           :key (:key data)
                           :name (:name data)
                           :bucket bucket
                           :slot slot}
                           (apply using opts)) idx))))

(defn fmt-del-kattr [data]
  (cons (delete-query :k_attr (where [[= :key (:key data)] [= :name (:name data)]]))
        (fmt-del-index :k_index {:key (:key data)
                                 :name (:name data)})))

(defn fmt-del-tattr [data]
  (if (contains? data :time)
    (let [[month bucket slot] (decode-time (:time data))]
      [(delete-query
        (t-attr-colfam month)
        (where [[= :key (:key data)]
                [= :name (:name data)]
                [= :bucket bucket]
                [= :slot slot]]))])
    (fmt-del-index :t_index {:key (:key data)
                             :name (:name data)})))

(defn put-index [cluster table indexes]
  (let [query (->> indexes
                   (map (partial fmt-put-index table))
                   (apply queries)
                   (batch-query (logged false)))]
    (client/execute cluster (client/render-query query))))

(defn getguid0 [cluster user tree kind node]
  (map (fn [row] [(:guid row) (:node row)])
       (select cluster
               :n_naming
               (columns :node :guid)
               (where [[= :user user] [= :tree tree] [= :kind kind] [= :node node]]))))

(defn getguid [cluster user tree kind node]
  (with-limit 1
    (if-let [[guid _] (first (getguid0 cluster user tree kind node))]
      guid)))

(defn getname [cluster guid]
  (first (map (fn [row] [(:user row) (:tree row) (:kind row) (:node row)])
              (select cluster
                      :g_naming
                      (columns :user :tree :kind :node)
                      (where [[= :guid guid]])
                      (limit 1)))))

(defn putguid [cluster user tree kind node]
  (if-let [guid (getguid cluster user tree kind node)]
    guid
    (let [guid (f/uuid-1)]
      (client/execute cluster
                      (client/render-query
                       (batch-query
                        (queries (insert-query :n_naming {:user user :tree tree :kind kind :node node :guid guid})
                                 (insert-query :g_naming {:user user :tree tree :kind kind :node node :guid guid})))))
      guid)))

(defn get-index [cluster table k & optional]
  (let [[start finish] optional]
    (map #(:name %)
         (select cluster
                 table
                 (columns :name)
                 (case [(boolean start) (boolean finish)]
                   [true true] (where [[= :key k] [>= :name start] [< :name finish]])
                   [true false] (where [[= :key k] [>= :name start]])
                   (where [[= :key k]]))
                 (limit +limit+)))))

(defn has-index [cluster table k name]
  (map #(:name %) (select cluster
                          table
                          (columns :name)
                          (where [[= :key k] [= :name name]])
                          (limit 1))))

(defn putlink [cluster links]
  (let [query (->> links
                   (map fmt-put-link)
                   (apply queries)
                   (batch-query (logged false)))]
    (client/execute cluster (client/render-query query))))

(defn dellink [cluster links]
  (let [query (->> links
                   (map fmt-del-link)
                   (apply queries)
                   (batch-query (logged false)))]
    (client/execute cluster (client/render-query query))))

(defn getlink [cluster k l & page]
  (map #(:b %) (select cluster
                       :graph
                       (columns :b)
                       (if (seq page)
                         (where [[= :a k] [= :l l] [>= :b (first page)]])
                         (where [[= :a k] [= :l l]]))
                       (limit +limit+))))

(defn put-tattr [cluster attrs]
  (let [query (->> attrs
                   (mapcat fmt-put-tattr)
                   (apply queries)
                   (batch-query (logged false)))]
    (client/execute cluster (client/render-query query))))

(defn get-tattr [cluster k name time]
  (let [[month bucket slot] (decode-time time)]
    (map
     (fn [row]
       [(encode-time bucket (:slot row)) (f/binary-to-bytes (:data row))])
     (select cluster
             (t-attr-colfam month)
             (columns :slot :data)
             (where [[= :key k] [= :name name] [= :bucket bucket] [>= :slot slot]])))))

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
                   (batch-query (logged false)))]
    (client/execute cluster (client/render-query query))))

(defn get-kattr [cluster k name]
  (first
   (map #(f/binary-to-bytes (:value %))
        (select cluster
                :k_attr
                (columns :value)
                (where [[= :key k] [= :name name]])
                (limit 1)))))

(defn del-kattr [cluster attrs]
  (let [query (->> attrs
                   (mapcat fmt-del-kattr)
                   (apply queries)
                   (batch-query (logged false)))]
    (client/execute cluster (client/render-query query))))
