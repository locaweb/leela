;; This file is part of Leela.
;;
;; Leela is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Leela is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Leela.  If not, see <http://www.gnu.org/licenses/>.

(ns leela.blackbox.storage.cassandra
  (:use     [clojure.tools.logging :only [info warn]]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [clojure.string :as s]
            [leela.blackbox.f :as f]
            [leela.blackbox.config :as cfg]
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
    (create-table cluster :graph (column-definitions {:a :blob :b :blob :primary-key [:a :b]})))
  (when-not (describe-table cluster keyspace :search)
    (warn "creating table search")
    (create-table cluster :search (column-definitions {:key :blob :code :int :name :varchar :primary-key [[:key :code] :name]}))))

(defmacro with-connection [[conn endpoint] & body]
  `(let [seed# (clojure.string/split ~endpoint #",")
         ~conn (client/connect (client/build-cluster {:contact-points seed#}))]
     (try
       ~@body
       (finally (.shutdown ~conn)))))

(defmacro with-session [[cluster endpoint keyspace] & body]
  `(with-connection [~cluster ~endpoint]
     (check-schema ~cluster ~keyspace)
     ~@body))

(defmacro with-consistency [tag & body]
  `(client/with-consistency-level
     (client/consistency-level ~tag) ~@body))

(defmacro with-limit [lim & body]
  `(with-redefs [+limit+ (or ~lim +limit+)]
     ~@body))

(defn truncate-all [cluster]
  (doseq [t [:graph :search]]
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
  (first (map #(:name %) (select cluster
                                 :search
                                 (columns :name)
                                 (where :key (f/hexstr-to-bytes k) :code code :name name)
                                 (limit 1)))))

(defn putlink [cluster a b]
  (insert cluster :graph {:a (f/hexstr-to-bytes a) :b (f/hexstr-to-bytes b)}))

(defn getlink [cluster k & page]
  (map #(f/bytes-to-hexstr (:b %)) (select cluster
                                           :graph
                                           (columns :b)
                                           (if (seq page)
                                             (where :a (f/hexstr-to-bytes k) :b [:>= (f/hexstr-to-bytes (first page))])
                                             (where :a (f/hexstr-to-bytes k)))
                                           (limit +limit+))))
