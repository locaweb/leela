(ns leela.blackbox.storage.cassandra
  (:use     [clojurewerkz.cassaforte.query]
            [clojurewerkz.cassaforte.multi.cql])
  (:require [clojure.data.json :as json]
            [clojurewerkz.cassaforte.client :as client]))

(def zero (byte-array []))

(defn hex-to-bytes [s]
  (org.apache.commons.codec.binary.Hex/decodeHex (.toCharArray (.substring s 2))))

(defn bytes-to-hex [b]
  (let [buffer (byte-array (.remaining b))]
    (.get b buffer)
    (str "0x" (String. (org.apache.commons.codec.binary.Hex/encodeHex buffer)))))

(defn create-leela-keyspace [session keyspace]
  (create-keyspace session keyspace
                   (with {:replication {:class "SimpleStrategy"
                                        :replication_factor 1}}))
  (use-keyspace session keyspace)
  (client/execute session "CREATE TABLE graph ( a blob , b blob , l varchar , PRIMARY KEY (a, b)) WITH caching     = 'ROWS_ONLY'AND compaction  = {'class': 'LeveledCompactionStrategy', 'sstable_size_in_mb': 10} AND compression = {'sstable_compression': 'SnappyCompressor'}")
  (client/execute session "CREATE TABLE t_props ( ref   blob , slot  int , data  blob , time  bigint , PRIMARY KEY ((ref, slot), time)) WITH caching     = 'NONE'AND compaction  = {'class': 'LeveledCompactionStrategy', 'sstable_size_in_mb': 32} AND compression = {'sstable_compression': 'SnappyCompressor'} AND CLUSTERING ORDER BY (time DESC)"))


(defn session [endpoints keyspace creat]
  (let [cluster (client/build-cluster {:contact-points endpoints})
        session (client/connect cluster)]
    (if-not (describe-keyspace session keyspace)
      (do (when-not creat (throw (IllegalArgumentException. "refusing to re-create keyspace")))
          (create-leela-keyspace session keyspace))
      (do (when creat (throw (IllegalArgumentException. "refusing to create keyspace [creat flag is false]")))
          (use-keyspace session keyspace)))
    session))

(defmacro with-consistency [tag & body]
  `(client/with-consistency-level
     (client/consistency-level ~tag) ~@body))

(defn proper-nodes [rows]
  (map (fn [row] [(bytes-to-hex (:b row)) (:l row)]) (filter #(> (.remaining (:b %)) 0) rows)))

(defn put-link [session links]
  (doseq [[a b l] links]
    (when-not (empty? b)
      (client/prepared (insert session
                               :graph
                               {:a (hex-to-bytes a) :b (hex-to-bytes b) :l l})))))

(defn put-node [session n k g]
  (client/prepared (insert session
                           :graph
                           {:a (hex-to-bytes g) :b zero :l (json/write-str [n k])})))

(defn get-name [session k]
  (map json/read-str (client/prepared (select session
                                              :graph
                                              (columns :l)
                                              (where :a (hex-to-bytes k) :b zero)))))

(defn get-node [session k]
  (proper-nodes (client/prepared (select session
                                         :graph
                                         (columns :b :l)
                                         (where :a (hex-to-bytes k))))))
