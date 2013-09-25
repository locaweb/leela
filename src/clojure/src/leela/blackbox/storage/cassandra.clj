(ns leela.blackbox.storage.cassandra
  (:use     [clojure.tools.logging :only [warn]]
            [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [leela.blackbox.config :as cfg]
            [clojurewerkz.cassaforte.client :as client]))

(def +label+ 0x0)

(def +name+  0x1)

(defn hex-to-bytes [s]
  (org.apache.commons.codec.binary.Hex/decodeHex (.toCharArray (.substring s 2))))

(defn bytes-to-hex [b]
  (let [buffer (byte-array (.remaining b))]
    (.get b buffer)
    (str "0x" (String. (org.apache.commons.codec.binary.Hex/encodeHex buffer)))))

(defn check-schema [cluster]
  (let [keyspace (:keyspace (cfg/read-state :cassandra))]
    (when-not (describe-keyspace cluster keyspace)
      (warn (format "creating keyspace %s [simplestrategy, rf=1]" keyspace))
      (create-keyspace cluster keyspace (with {:replication {:class "SimpleStrategy" :replication_factor 1}})))
    (use-keyspace cluster keyspace)
    (when-not (describe-table cluster keyspace :graph)
      (warn "creating table graph")
      (create-table cluster :graph (column-definitions {:a :blob :b :blob :primary-key [:a :b]})))
    (when-not (describe-table cluster keyspace :search)
      (warn "creating table search")
      (create-table cluster :search (column-definitions {:key :blob :code :int :name :varchar :primary-key [[:key :code] :name]})))))

(defmacro with-connection [[conn] & body]
  `(let [cfg#  (cfg/read-state :cassandra)
         ~conn (client/connect (client/build-cluster {:contact-points (:seed cfg#)}))]
     (try
       ~@body
       (finally (.shutdown ~conn)))))

(defmacro with-session [[cluster] & body]
  `(with-connection [~cluster]
     (check-schema ~cluster)
     ~@body))

(defmacro with-consistency [tag & body]
  `(client/with-consistency-level
     (client/consistency-level ~tag) ~@body))

(defn truncate-all [cluster]
  (doseq [t [:graph :search]]
    (truncate cluster t)))

(defn putlink [cluster a links]
  (let [k (hex-to-bytes a)]
    (doseq [b links]
      (insert cluster :graph {:a k :b (hex-to-bytes b)}))))

(defn putlabel [cluster a labels]
  (let [k (hex-to-bytes a)]
    (doseq [l labels]
      (insert cluster :search {:key k :code +label+ :name l}))))

(defn putname [cluster n g]
  (insert cluster :search {:key (hex-to-bytes g) :code +name+ :name n}))

(defn getname [cluster k]
  (let [cfg (cfg/read-state :cassandra)
        raw (select
             cluster
             :search
             (columns :name)
             (where :key (hex-to-bytes k) :code +name+)
             (limit 1))]
    (when (seq raw) (:name (first raw)))))

(defn getlink [cluster k]
  (let [cfg (cfg/read-state :cassandra)]
    (map #(bytes-to-hex (:b %)) (select
                                 cluster
                                 :graph
                                 (columns :b)
                                 (where :a (hex-to-bytes k))
                                 (limit (:rows-limit cfg))))))

(defn getlabel [cluster k]
  (let [cfg (cfg/read-state :cassandra)]
    (map #(:name %) (select
                     cluster
                     :search
                     (columns :name)
                     (where :key (hex-to-bytes k) :code +label+)
                     (limit (:rows-limit cfg))))))
