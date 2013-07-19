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

(defn session [endpoints keyspace]
  (client/connect (client/build-cluster {:contact-points endpoints}) keyspace))

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
