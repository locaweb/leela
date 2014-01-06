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
     (column-definitions {:a :blob :b :blob :primary-key [:a :b]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :tsattr)
    (warn "creating table tsattr")
    (create-table
     cluster :tsattr
     (column-definitions {:key :blob :slot (map-type :int :blob)  :primary-key [:key]})
     (with {:compaction {:class "LeveledCompactionStrategy" :sstable_size_in_mb "128"}})))
  (when-not (describe-table cluster keyspace :search)
    (warn "creating table search")
    (create-table
     cluster
     :search
     (column-definitions {:key :blob :code :int :name :varchar :primary-key [[:key :code] :name]})
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
  (doseq [t [:graph :search :tsattr]]
    (truncate cluster t)))

(defn putindex [cluster k code name]
  (insert cluster :search {:key (f/hexstr-to-bytes k) :code code :name name}))

(defn getindex [cluster k code & optional]
  (let [[start finish] optional]
    (map #(:name %) (select cluster
                            :search
                            (columns :name)
                            (case [(boolean start) (boolean finish)]
                              [true true] (where :key (f/hexstr-to-bytes k) :code code :name [:>= start] :name [:< finish])
                              [true false] (where :key (f/hexstr-to-bytes k) :code code :name [:>= start])
                              (where :key (f/hexstr-to-bytes k) :code code))
                            (limit +limit+)))))

(defn hasindex [cluster k code name]
  (map #(:name %) (select cluster
                          :search
                          (columns :name)
                          (where :key (f/hexstr-to-bytes k) :code code :name name)
                          (limit 1))))

(defn putlink [cluster a b]
  (insert cluster :graph {:a (f/hexstr-to-bytes a) :b (f/hexstr-to-bytes b)}))

(defn dellink [cluster a & b]
  (delete cluster
           :graph
           (if (seq b)
             (where :a (f/hexstr-to-bytes a) :b (f/hexstr-to-bytes (first b)))
             (where :a (f/hexstr-to-bytes a)))))

(defn getlink [cluster k & page]
  (map #(f/bytes-to-hexstr (:b %)) (select cluster
                                           :graph
                                           (columns :b)
                                           (if (seq page)
                                             (where :a (f/hexstr-to-bytes k) :b [:>= (f/hexstr-to-bytes (first page))])
                                             (where :a (f/hexstr-to-bytes k)))
                                           (limit +limit+))))

(defn putattr [cluster k timest value]
  (update cluster
          :tsattr {:slot [+ {timest (f/hexstr-to-bytes value)}]}
          (where :key (f/hexstr-to-bytes k))))

(defn getattr [cluster k & timest]
  (if (boolean timest)
    (get (into {}
               (first
                 (map #(:slot %) (select cluster
                                         :tsattr
                                         (columns :slot)
                                         (where :key (f/hexstr-to-bytes k))
                                         )))) timest)
    (into {} (first (map #(:slot %) (select cluster
                                            :tsattr
                                            (columns :slot)
                                            (where :key (f/hexstr-to-bytes k))
                                            ))))
    ))

(defn delattr [cluster k]
  (delete cluster
          :tsattr
          (where :key (f/hexstr-to-bytes k))))
